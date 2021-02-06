package utils.extraction

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.ImageType
import org.apache.pdfbox.rendering.PDFRenderer
import org.apache.pdfbox.tools.imageio.ImageIOUtil
import java.awt.image.BufferedImage
import java.io.File
import java.io.IOException

import ammonite.ops.Path
import shared.FileWithContent
import app.utils.FileUtils


object PdfToImage {
  val isDebug = false
  //val OUTPUT_DIR = "/tmp/"
  val OUTPUT_DIR = if (System.getenv("APP_TMP_FOLDER_PATH") != null) {
		System.getenv("APP_TMP_FOLDER_PATH")
	} else "/tmp/"
  val USE_HOCR = true

  def makeName(path: String): String = path.split("/").reverse.headOption.getOrElse("")

  def isPdfFile(content: String): Boolean = !content.contains("Pdf reading is not supported")

  def runTesseractCmd(filename: String): Either[String, String] = {
    val tokens = filename.split("/").reverse.headOption.getOrElse("").split("""\.""")
    val outfilename: String = tokens.headOption.map(s => s"${OUTPUT_DIR}${s}_output.${tokens.reverse.head}").getOrElse("")
    val inFile: Path = Path(filename)
    val outFile: Path = Path(outfilename)
    val cmd = "tesseract"
    val args = Seq(filename, outfilename)
    println(s"Running runTesseractCmd: $cmd $args")
    utils.ShellUtils.execute(cmd, args)
  }


  //Right(filename, content)
  def extractUsingTesseract(filename: String): Either[String, FileWithContent] = {
    val tokens = filename.split("/").reverse.headOption.getOrElse("").split("""\.""")
    val ext = tokens.reverse.headOption.getOrElse("")
    val outfilename: String = tokens.headOption.map(s => s"${OUTPUT_DIR}${s}_output.txt").getOrElse("")
    //val outfilename: String = tokens.headOption.map(s => s"${OUTPUT_DIR}${s}_output.${tokens.reverse.head}").getOrElse("")
    val inFile: Path = Path(filename)
    val outFile: Path = Path(outfilename)
    val cmd = "tesseract"
    val isViaProcessBuilder = true
    //val args = Seq(filename, outfilename)
    val args = if (USE_HOCR) Seq(filename, outfilename, "-l", "eng", "hocr") else Seq(filename, outfilename)
    val contentFilename: String = if (USE_HOCR) s"${outfilename}.hocr" else s"${outfilename}.txt"
    println(s"Running extractUsingTesseract cmd: $cmd $args and contentFilename: $contentFilename")
    val output = if (USE_HOCR) utils.ShellUtils.executeViaProcessBuilder(cmd, args) else utils.ShellUtils.execute(cmd, args)
    output match {
      case Right(value) =>
        if (isDebug)
          println(value)
        val bytes = FileUtils.getFileSize(filename)
        val size = FileUtils.getFileInKiloBytes(filename)
        Right(FileWithContent(makeName(contentFilename), contentFilename,app.utils.Utils.getFileContent(contentFilename).getOrElse(""), ext, true, FileWithContent.OCR, bytes = Some(bytes), size = Some(size)))
      case Left(error) =>
        if (isDebug)
        println(error)
        Left(error)
    }
  }


  //Tesseract Open Source OCR Engine v4.1.0 with Leptonica
  //Error in pixReadStream: Pdf reading is not supported
  //Error in pixRead: pix not read
  //Error during processing.
  def checkImagesInPDF(filename: String): Boolean = {
    val pdfType = filename.toLowerCase.endsWith(".pdf")
      runTesseractCmd(filename) match {
        case Right(value) =>
          if (isDebug)
          println(value)
          if (isDebug)
          println(s"PDF: ${isPdfFile(value)}")
          true
        case Left(error) =>
          if (isDebug)
          println(error)
          if (isDebug)
          println(s"PDF error: ${isPdfFile(error)}")
          false
      }
  }


  val MAX_PAGES_PER_VALID_DOC = 25
  def parse(filename: String, noOfPages: Option[Int] = None):Either[String, FileWithContent] = {
    try {
      val bytesV = FileUtils.getFileSize(filename)
      val size = FileUtils.getFileInKiloBytes(filename)
      if (bytesV <= 8.0) {
        Right(FileWithContent(makeName(filename), path = filename, content = "", "pdf", false, FileWithContent.OCR, bytes = Some(bytesV), size = Some(size)))
      } else {
        if (bytesV >= 15728640) {
          Right(FileWithContent(makeName(filename), path = filename, content = "INVALID", "pdf", false, FileWithContent.OCR, bytes = Some(bytesV), size = Some(size)))
        } else {

          val hasImages = checkImagesInPDF(filename)
          val tokens = filename.split("/").reverse.headOption.getOrElse("").split("""\.""")
          val ext = tokens.reverse.headOption.getOrElse("")
          val document: PDDocument = PDDocument.load(new File(filename))
          val pdfRenderer: PDFRenderer = new PDFRenderer(document)
          val pagesCount = document.getNumberOfPages
          val actualPageCount = if (noOfPages.isDefined && (noOfPages.getOrElse(0) < pagesCount)) noOfPages.getOrElse(0) else pagesCount
          if (isDebug) {
            println(s"Document, $filename has page count = ${pagesCount} and effective page count = ${actualPageCount}")
          }
          if (pagesCount >= MAX_PAGES_PER_VALID_DOC) {
            document.close
            Right(FileWithContent(makeName(filename), path = filename, content = s"Seems like invalid document because no of pages found = $pagesCount", "pdf", true, FileWithContent.OCR, bytes = Some(bytesV), size = Some(size), noOfPages = Some(pagesCount)))
          } else {
            val files: Seq[String] = (1 to actualPageCount).map(page => {
              val bi: BufferedImage = pdfRenderer.renderImageWithDPI(page - 1, 300, ImageType.RGB)
              val outfilename: String = tokens.headOption.map(s => s"${OUTPUT_DIR}${s}_output_$page.png").getOrElse("")
              //val fileName = OUTPUT_DIR + "image-out-" + page + ".png"
              import org.apache.pdfbox.tools.imageio.ImageIOUtil
              ImageIOUtil.writeImage(bi, outfilename, 300)
              outfilename
            })
            document.close
            if (isDebug)
              println(s"Image files from PDF = $files")
            val bytes = FileUtils.getFileSize(filename)
            val size = FileUtils.getFileInKiloBytes(filename)
            val contents: Seq[Either[String,shared.FileWithContent]] = files.map(file => {
              extractUsingTesseract(file)
            })
            val errors = contents.filter(f => f.isLeft).map(_.left.get)
            val filesDatas = contents.filter(f => f.isRight).map(_.right.get)
            val fileWithContentOk = FileWithContent(name = makeName(filename), path = filename, content = filesDatas.map(_.content).mkString("""\n"""), ext, true, FileWithContent.OCR, bytes = Some(bytes), size = Some(size), noOfPages = Some(pagesCount))
            val fileWithContentError = errors.mkString("""\n""")
            if (errors.isEmpty) Right(fileWithContentOk) else Right(FileWithContent(makeName(filename), path = filename, content = "", ext, false, FileWithContent.OCR, bytes = Some(bytes), size = Some(size)))
          }
        }

      }
    } catch {
      case ex: IOException =>
        if (isDebug)
        ex.printStackTrace()
        Left(app.utils.Utils.stackTrace(ex))
      case _ @ ex: Throwable =>
        if (isDebug)
        println(ex)
        Left(app.utils.Utils.stackTrace(ex))
    }
  }

}
