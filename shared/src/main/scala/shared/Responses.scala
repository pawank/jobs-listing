package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

sealed trait AppResult {
}

case class SuccessResponse(code: Int,
                   message: String = "",
                    status: Boolean = true,
                 exception: Option[String] = None, link: Option[String] = None, session: Option[String] = None) extends AppResult

case class ErrorResponse(code: Int,
                 message: String,
                  status: Boolean = false,
  exception: Option[String] = None) extends AppResult

object SuccessResponse {
  implicit val jsonFormatSuccess: Format[SuccessResponse] = Json.format[SuccessResponse]
}

object ErrorResponse {
  implicit val jsonFormatError: Format[ErrorResponse] = Json.format[ErrorResponse]
}
