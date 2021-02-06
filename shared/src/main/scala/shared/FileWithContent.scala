package shared

import java.time.ZonedDateTime

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class FileWithContent(name: String, path: String, content: String, ext: String,
                           valid: Boolean, parserName: String,
                           bytes: Option[Double],
                           size: Option[String], noOfPages: Option[Int] = None)


object FileWithContent {
  implicit val jsonFormatFile: Format[FileWithContent] = Json.format[FileWithContent]

  val OCR = "ocr"
  val PDFBOX = "pdfbox"
  val TIKA = "tika"
  val OTHER = "other"
}

