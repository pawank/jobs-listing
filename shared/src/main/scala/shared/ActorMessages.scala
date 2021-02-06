package shared

sealed trait Message

case class Run(path: String) extends Message

case class RunWithSource(source: SourceMetadata, parsing: Boolean = true, id: Option[String] = None, fetchAllParams: Boolean = false) extends Message

case class ExecuteAndDie(source: SourceMetadata) extends Message

case class Process(url: String) extends Message

case class DownloadFile(url: String, targetName: String) extends Message

case class StartCrawlingSingleSite(site: SiteCrawler, cacheFolder: String) extends Message

case class StartCrawlingSites(sites: List[SiteCrawler], cacheFolder: String) extends Message

case class Experiment(path: String) extends Message

case class LoadExcel(path: String) extends Message

case class ParseFullJob(job: FullJob, employers: Seq[Employer], matches: Seq[MatchingRule]) extends Message

case class FetchAndParse(urls: List[String]) extends Message

case object Stop extends Message

