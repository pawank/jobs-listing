package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._


case class NameValue(name: String, value: String)

object NameValue {
  implicit val jsonFormatNameValue: Format[NameValue] = Json.format[NameValue]
}


case class DropdownValue(name: String, value: String, text: String,disabled: Boolean = false)

object DropdownValue {
  implicit val jsonFormatNameDropdown: Format[DropdownValue] = Json.format[DropdownValue]
}

case class DropdownResponse(success: Boolean, results: Seq[DropdownValue])
object DropdownResponse {
  implicit val jsonFormatNameDropdownResponse: Format[DropdownResponse] = Json.format[DropdownResponse]
}


case class IDNameValue(_key: Option[String], id: Option[String], name: String, value: String) extends ID

object IDNameValue {
  implicit val jsonFormatNameValueID: Format[IDNameValue] = Json.format[IDNameValue]
}

