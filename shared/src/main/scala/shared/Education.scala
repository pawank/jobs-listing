package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class EducationTag(name: String, mainTag: String, category: String, level: String, priority: Int = 0, tags: Option[List[String]] = None,  _key: Option[String] = None) {
  override def toString: String = {
    s"""Category:$category, domain:$mainTag, level:$level,name:$name\n"""
  }
}

object EducationTag {
  implicit val jsonFormatEducationTag: Format[EducationTag] = Json.format[EducationTag]
}

case class Education(id: Option[String], _key: Option[String], code: String, name: String, tags: List[String], category: Option[String] = None, level: Option[String] = None, description: Option[String] = None) extends ID {
  def toMatchingRule() = {
    MatchingRule(id = id, _key = _key, name = name, rule = code, matches = tags, category = category.getOrElse(""), level = level, matchType = "Education", description = description)
  }

  def toEducationTag() = {
    EducationTag(name = name, mainTag = name, category = category.getOrElse(""), level = level.getOrElse(""), tags = Some(tags))
  }
}

object Education {
  implicit val jsonFormatEducation: Format[Education] = Json.format[Education]
}

