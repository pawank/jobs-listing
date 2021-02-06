package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class Address(name: String, line1: Option[String], line2: Option[String], line3: Option[String], addrType: Option[String], location: Option[Location])

object Address {
  implicit val jsonFormatAddress: Format[Address] = Json.format[Address]
}
