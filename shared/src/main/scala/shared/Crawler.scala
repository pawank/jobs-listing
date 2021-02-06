package shared

import java.time.{Duration, ZonedDateTime}

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class CrawlingRule(fileTypes: Seq[String])
object CrawlingRule {
  implicit val jsonFormatSiteCrawlerRule: Format[CrawlingRule] = Json.format[CrawlingRule]
}


trait Crawler {
  def name: String
  def url: String
  def rules: List[CrawlingRule]
}

case class SiteCrawlerResult(sourceUrl: String,
                             links: Seq[String],
                             labels: Seq[String],
                             names: Seq[String],
                             paths: Seq[String],
                             s3paths: Seq[String],
                             negativeMatches: Option[String],
                             positiveMatches: Option[String],
                             blob: Option[String],
                             totalCount: Option[Int] = None,
                             validUrlCount: Option[Int] = None,
                             dates: DatesInfo = DatesInfo.getCurrentDatesInUTC(),
                             simpleJobs: Option[Seq[SimpleJob]] = None,
                             started: Option[ZonedDateTime] = None,
                             completed: Option[ZonedDateTime] = None,
                             status: Option[String] = None)
object SiteCrawlerResult{
  implicit val jsonFormatSiteCrawlerResult: Format[SiteCrawlerResult] = Json.format[SiteCrawlerResult]
  def empty = SiteCrawlerResult(sourceUrl = "", links = Seq.empty, labels = Seq.empty, names = Seq.empty, paths = Seq.empty, s3paths = Seq.empty, blob = None, negativeMatches = None, positiveMatches = None)
  def getSiteCrawlerResult(name: String, url: String): SiteCrawlerResult = {
    empty.copy(sourceUrl = url, links = Seq(url), names = Seq(name), paths = Seq(url), s3paths = Seq(url), labels = Seq(name))
  }
}


case class SiteCrawler(
                        id: Option[String] = None,
                        _key: Option[String] = None,
                        name: String,
                        url: String,
                        priority: Int,
                        domain: Option[String],
                        status: String,
                        result: Option[SiteCrawlerResult],
                        siteType: Option[String],
                        ignore: Option[String],
                        originalUrl: Option[String] = None,
                        noAds: Option[Int] = None,
                        noJobs: Option[Int] = None,
                        fullJobStatus: Option[String] = Some(""),
                        source: Option[String] = None,
                        output: Option[String] = None,
                        jobStatus: Option[String] = Some("")
                      ) extends ID {

  def getS3paths() = {
    result.map(_.paths).getOrElse(
      Seq.empty
    ).mkString(" ")
  }

  def getJobSearch() = {
    s""""$url""""
  }

  def getResult() = {
    output match {
      case Some(out) =>
        s"""Ads = ${noAds}, jobs = ${noJobs} and paths = ${getS3paths()}"""
      case _ =>
        s"""no Ads = ${noAds} and no jobs = ${noJobs}"""
    }
  }

  def getTimeTaken(): String = {
    result match {
      case Some(r) =>
        (Duration.between(r.started.getOrElse(ZonedDateTime.now()), r.completed.getOrElse(ZonedDateTime.now())).toMillis / 1000).toString
      case _ => ""
    }
  }
}

object SiteCrawler{
  implicit val jsonFormatSiteCrawler: Format[SiteCrawler] = Json.format[SiteCrawler]

  def toSourceMetadataList(site: SiteCrawler): Seq[SourceMetadata] = {
          val urls1: Seq[SourceMetadata]  = site.result.map(ss => ss.paths.zipWithIndex.map(c => {
            val link = if (ss.links.size >= ss.paths.size) ss.links(c._2) else ""
            val s3path = if (ss.s3paths.size >= ss.s3paths.size) ss.s3paths(c._2) else ""
            val name = if (ss.names.size >= ss.names.size) ss.names(c._2) else ""
            val path = if (ss.paths.size >= ss.paths.size) ss.paths(c._2) else ""
            val label = if (ss.labels.size >= ss.paths.size) ss.labels(c._2) else ""
            val publicUrl = if (!s3path.isEmpty) Some(s3path) else Some(link)
            SourceMetadata(path = c._1, url = Some(link), sourceUrl = Some(ss.sourceUrl), title = Some(label), downloadPath = Some(path), s3Path = Some(s3path), publicUrl = publicUrl, negativeMatches = ss.negativeMatches, positiveMatches = ss.positiveMatches)
          })).getOrElse(Seq.empty)
          val urls2: Seq[SourceMetadata] = {
            val link = site.originalUrl.getOrElse(site.url)
            //val path = "pdf/" + site.name
            val path = link
            Seq(SourceMetadata(path = "", url = Some(link), sourceUrl = Some(link), title = None, downloadPath = None, s3Path = None, publicUrl = None, negativeMatches = None, positiveMatches = None))
          }
          urls1 ++ urls2
        }
}


case class SiteCrawlerDetail(name: String, url: String, priority: Int, rules: List[CrawlingRule] = List.empty, dates: DatesInfo, completed: Option[ZonedDateTime])
object SiteCrawlerDetail{
  implicit val jsonFormatSiteCrawlerDetail: Format[SiteCrawlerDetail] = Json.format[SiteCrawlerDetail]

  def fromSiteCrawler(obj: SiteCrawler): SiteCrawlerDetail = {
    SiteCrawlerDetail(name = obj.name, url = obj.url, priority = obj.priority, rules = List.empty, dates = DatesInfo.getCurrentDatesInUTC(), completed = None)
  }
}

