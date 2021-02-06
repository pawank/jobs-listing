package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class MatchingRule(id: Option[String], _key: Option[String], rule: String, name: String,
                        matches: List[String], matchType: String, category: String, level: Option[String] = None,
                        description: Option[String] = None, slug: Option[String] = None) extends ID {
  def toEducation() = {
    Education(id = None, _key = None, name = name, code = rule, tags = matches.toSeq, category = Some(category), level = level, description = description)
  }
}

object MatchingRule {
  implicit val jsonFormatMatchingRule: Format[MatchingRule] = Json.format[MatchingRule]

}

