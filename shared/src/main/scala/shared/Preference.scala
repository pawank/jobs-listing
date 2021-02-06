package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

sealed trait MyPreference extends ID {
  def jobId: String
  def dates:Option[DatesInfo]
}

//siteName should be unique
case class Preference(id: Option[String], _key: Option[String], jobId: String,
                      jobTypes: List[String], locations: List[String], skills: List[String], employers: List[String],
                      email: Option[String], secret: Option[String],
                      query: Option[String], filters: Option[String],
                      dates: Option[DatesInfo] = Some(DatesInfo.getCurrentDatesInUTC())) extends MyPreference

object Preference {
  implicit val jsonFormatMyPreference: Format[Preference] = Json.format[Preference]
}

