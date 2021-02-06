package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

sealed trait Page extends ID {
  def title: String
  def description: String
  def siteName: String
  def tags: List[String]
  def dates:Option[DatesInfo]
}

case class PageCondition(name: String, value: String)

case class SeoTags(meta1: String, meta2: String, meta3: String, meta4: String, meta5: String)

//siteName should be unique
case class SitePage(id: Option[String], _key: Option[String],
                    title: String,
                    description: String,
                    siteName: String,
                    image: Option[String],
                    pageType: String,
                    condition: String,
                    url: Option[String],
                    seo: SeoTags,
                    tags: List[String],
                    bannerTitle: Option[String],
                    banner: Option[String], slug: Option[String],
                    dates: Option[DatesInfo]) extends Page
object PageCondition {
  implicit val jsonFormatPageCond: Format[PageCondition] = Json.format[PageCondition]
  val conditions = List(
    PageCondition(name = "State", value = "state"),
    PageCondition(name = "State Code", value = "state-code"),
    PageCondition(name = "Central Government", value = "center"),
    PageCondition(name = "Location", value = "location"),
    PageCondition(name = "Profession", value = "profession"),
    PageCondition(name = "Employer", value = "employer"),
    PageCondition(name = "Industry", value = "industry"),
    PageCondition(name = "Education Any", value = "education"),
    PageCondition(name = "Education in State", value = "education-in-state"),
    PageCondition(name = "Education in Central Government", value = "education-in-center"),
    PageCondition(name = "Education in District", value = "education-in-district"),
    PageCondition(name = "Education in Location", value = "education-in-location"),
    PageCondition(name = "Profession in State", value = "profession-in-state"),
    PageCondition(name = "Profession in Central Government", value = "profession-in-center"),
    PageCondition(name = "Profession in District", value = "profession-in-district"),
    PageCondition(name = "Profession in Location", value = "profession-in-location"),
  )
}

object SeoTags {
  implicit val jsonFormatPageSeotags: Format[SeoTags] = Json.format[SeoTags]
  val default = SeoTags(meta1 = "", meta2 = "", meta3 = "", meta4 = "", meta5 = "")
}

object SitePage {
  implicit val jsonFormatPage: Format[SitePage] = Json.format[SitePage]

  def default: SitePage = SitePage(id = None, _key = None, condition = "", title = "", description = "", siteName = "", url = None, image = None, pageType = "", tags = List.empty, bannerTitle = None, banner = None, slug = None, seo = SeoTags.default, dates = Some(DatesInfo.getCurrentDatesInUTC()))
}

