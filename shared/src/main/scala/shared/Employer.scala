package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class EmployerExtra(category: String,  stateType: String, state: String, area: String, department: Option[String], industry: Option[String],
                         priority: Option[Int] = None, links: Option[List[String]] = None)
case class Employer(id: Option[String] = None,
                     _key: Option[String] = None,
                    hash: Option[String] = None,
                    name: String, code: String, website: String, domain: Option[String], extra: EmployerExtra, statusValue: Option[String] = Some("active")) extends ID

object EmployerExtra {
  implicit val jsonFormatEmployerExtra: Format[EmployerExtra] = Json.format[EmployerExtra]

  val empty = EmployerExtra(category = "", stateType = "", state = "", area = "", department = None, industry = None)
}

object Employer {
  implicit val jsonFormatEmployer: Format[Employer] = Json.format[Employer]

  val empty = Employer(hash = None, name = "", code = "", website = "", domain = None, extra = EmployerExtra(category = "", stateType = "", state = "", area = "", department = None, industry = None))

  def getWebsite(website: String): String = {
      website
      .replaceAll("""https://""", "")
      .replaceAll("""http://""", "")
      .split("/")
      .toList
      .headOption
      .getOrElse("").replaceAll("""www.""","").toLowerCase
  }
}

