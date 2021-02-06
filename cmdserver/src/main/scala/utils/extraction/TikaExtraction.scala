package utils.extraction

import java.io.{File, FileInputStream, InputStream}

import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.ToXMLContentHandler
import org.xml.sax.ContentHandler
import shared.FileWithContent
import app.utils.FileUtils

object TikaExtraction {
  def makeName(path: String): String = path.split("/").reverse.headOption.getOrElse("")
  def parseAsHtml(filename: String, noOfPages: Option[Int] = None, parsing: Boolean = true): Either[String, FileWithContent] = {
    val data1: Either[String, FileWithContent] = if (parsing) {
        utils.extraction.PdfToImage.parse(filename, noOfPages)
    } else Left("PARSING DISABLED")
    val data2: Either[String, FileWithContent] = try {
      val tokens = filename.split("/").reverse.headOption.getOrElse("").split("""\.""")
      val ext = tokens.reverse.headOption.getOrElse("")
      val handler: ContentHandler = new ToXMLContentHandler()
          val parser: AutoDetectParser = new AutoDetectParser()
          //println(s"Tika parser used: ${parser.getParsers.toString}")
          val metadata: Metadata = new Metadata()
          val initialFile: File = new File(filename)
      if (initialFile.exists()) {
        val stream: InputStream = new FileInputStream(initialFile)
        val bytes = FileUtils.getFileSize(filename)
        val size = FileUtils.getFileInKiloBytes(filename)
        try {
          //Check for non empty pdf file
          if (bytes <= 8) {
          } else {
            parser.parse(stream, handler, metadata)
          }
          val content = handler.toString()
          Right(FileWithContent(makeName(filename), path = filename, content = content, ext = ext, valid = !content.isEmpty, FileWithContent.TIKA, bytes = Some(bytes), size = Some(size)))
        } catch {
          case e: Exception =>
            val error = app.utils.Utils.stackTrace(e)
            println(s"TikaExtraction parsing error = $error for filename = $filename")
            if (error.contains("ZeroByteFileException")) {
              Right(FileWithContent(makeName(filename), path = filename, content = "", ext = ext, valid = false, FileWithContent.TIKA, bytes = Some(bytes), size = Some(size)))

            } else {
              Left(error)
            }
        }
      } else {
        Right(FileWithContent(makeName(filename), path = filename, content = "", ext = ext, valid = false, FileWithContent.TIKA, bytes = Some(0.0), size = Some("0")))
      }
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(s"TikaExtraction parsing and file generation for parsed data error = $error for filename = $filename")
        Left(error)
    }
    (data1.isRight, data2.isRight) match {
      case (true, true) =>
        val content1 = data1.right.get.content
        val content2 = data2.right.get.content
        Right(if (content2.size > content1.size) data2.right.get else data1.right.get)
      case (true, false) =>
        //val content = data1.right.get.content
        Right(data1.right.get)
      case (false, true) =>
        //val content = data2.right.get.content
        Right(data2.right.get)
      case _ =>
        Left(data1.left.get + data2.left.get)
    }
  }


}
