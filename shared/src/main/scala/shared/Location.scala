package shared


import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class LatLng(latitude: Double, longitude: Double)
object LatLng {
  implicit val jsonFormatLatLng: Format[LatLng] = Json.format[LatLng]
}

case class Location(name: String, landmark: Option[String], geocode: Option[LatLng])
object Location {
  implicit val jsonFormatLocation: Format[Location] = Json.format[Location]
}


