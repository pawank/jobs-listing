package shared

import java.time.{LocalDate, ZonedDateTime}

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
//import shared.JavaReads.{defaultJavaLocalDateTimeReads, defaultJavaZonedDateTimeReads}
//import shared.JavaWrites.{localDateWrites, zonedDateTimeWrites, ZonedDateTimeWrites, ZonedDateTimeNumberWrites}
import shared.JavaReads._
import shared.JavaWrites._

case class JobStats(noPosts: Option[Int] = None, viewCount: Int = 0, searchCount: Int = 0, clickCount: Int = 0, ranking: Int = 0)

case class SiteUrl(url: String, title: String, whatFor: String) {
  override def toString = {
    val domain = SiteUrl.getJustDomain(url)
    s"$domain"
  }
  //override def toString = s"$whatFor link $url"

}

case class FullJob(
                    id: Option[String] = None,
                    _key: Option[String] = None,
                    source: SourceMetadata,
                    content: Option[FileWithContent],
                    originalLink: Option[String],
                    errors: Option[String] = None,
                    status: Option[String] = Some("Pending"),
                    googleSearchResult: Option[GoogleSearchResult] = None,
                    dates:DatesInfo = DatesInfo.getCurrentDatesInUTC()) extends ID

case class Keywords(positive: List[String], negative: List[String])

case class SimpleJob(board: String, originalPdf: String, postDate: String, qualification: String, title: String, advNo: String, lastDate: String,
                     url: String, walkIn: String, originalLabel: String = "",
                     startDate: Option[LocalDate] = None,
                     endDate: Option[LocalDate] = None,
                     links: Option[Seq[SiteUrl]] = None,
                     blob: Option[String] = None) {

  def printLinks(domainForFiltering: String) = {
    links.map(xs => xs.map(_.toString)).getOrElse(Seq.empty).filter(url => !SiteUrl.getJustDomain(url).toLowerCase.contains(domainForFiltering)).distinct.map(s => {
      s"""<a href="http://$s" target="_blank">$s</a>"""
    }).mkString("""<br/>""")
  }
}

case class JobExtra(originalPdf: String,
                    originalLabel: Option[String],
                    stats: Option[JobStats] = Some(JobStats()),
                    category: List[String] = List.empty,
                    docLink: Option[String] = None,
                    department: String,
                    education: Option[String] = None,
                    experience: Option[String] = None,
                    detail: String = "",
                    sector: Option[String] = None,
                    location: Option[Location] = None,
                    address: Option[Address] = None,
                    website: String,
                    keywords: Option[Keywords] = None,
                    employer: Option[Employer] = None,
                    educations: Seq[EducationTag] = Seq.empty,
                    employmentTypes: Seq[EducationTag] = Seq.empty,
                    possibleDates: Seq[LocalDate] = Seq.empty,
                    simpleJob: Option[SimpleJob] = None,
                    dates: DatesInfo)



case class JobSummary(
                     _key: Option[String] = None,
                     title: String,
                     company: String,
                     companyUrl: String,
                     educations: Seq[String],
                     refLink: String,
                     originalPdf: Option[String],
                     state: String,
                     jobType: Option[String] = None,
                     startDate: Option[LocalDate],
                     endDate: Option[LocalDate],
                     processType : Option[String] = None,
                     tags: Option[List[String]] = None,
                     publicUrl: Option[String],
                     sourceId: Option[String] = None,
                     status: String,
                     liked: Option[Boolean] = None,
                     educationTags: Option[Seq[String]] = None,
                     stats: Option[JobStats] = Some(JobStats())
                     ) {
  def getNoPosts(): Int = {
    stats.map(_.noPosts.getOrElse(0)).getOrElse(0)
  }
}

case class Job(
                id: Option[String] = None,
                _key: Option[String] = None,
                jobType: Option[String],
                title: String,
                company: String,
                description: Option[String] = None,
                startDate: Option[LocalDate],
                endDate: Option[LocalDate],
                educations: Seq[String],
                refLink: String,
                publicUrl: Option[String],
                tags: Option[List[String]] = None,
                category: Option[List[String]] = None,
                state: Option[String] = None,
                processType : Option[String] = None,
                sourceId: Option[String] = None,
                advPath: Option[String],
                content: Option[FileWithContent],
                extras: Option[JobExtra] = None,
                status: Option[String] = Some("Pending"),
                fetchStatus: Option[String] = None
              ) extends ID {

  def getNoPosts(): Int = {
    val no = for {
      extra <- extras
      stats <- extra.stats
    } yield stats.noPosts.getOrElse(0)
    no.getOrElse(0)
  }

  def printSimpleJobLinks(domain: String = Job.APP_BASE_DOMAIN) = {
    extras match {
      case Some(extras) =>
        extras.simpleJob.map(e => {
          val boardlink = s"""<a href="/backoffice/jobs?search=${e.board}">${e.board}</a>"""
          //val boardlink = s"""<a href="/backoffice/jobs?search=&quote;${e.board}&quote;">${e.board}</a>"""
          boardlink + """<br/>""" + e.printLinks(domain)
        }).getOrElse( "" )
      case _ => ""
    }
  }

  def getCompanyUrl = {
    extras.map(e => e.employer.map(_.website).getOrElse("")).getOrElse("")
  }

  def toJobSummary(likes: List[String]) = {
    val stats = extras.map(_.stats.getOrElse(JobStats())).getOrElse(JobStats())
    //println(s"Stats: $stats and original: ${extras.map(_.stats)}")
    val empState = extras.map(e => e.employer.map(emp => emp.extra.state).getOrElse("")).getOrElse("")
    val stateFinal = state.getOrElse(empState)
    val edcs = educations.filter(s => !s.equalsIgnoreCase("Level")).filter(edu => !edu.isEmpty)
    val curl = extras.map(e => e.employer.map(_.website).getOrElse("")).getOrElse("")
    //println(s"${_key} and educations = $edcs")
    val educationTags = extras.map(_.educations.map(_.tags.getOrElse(List.empty)).flatten.toList).getOrElse(List.empty).toSeq
    JobSummary(_key = _key, title = title, company = company, companyUrl = curl, educations = edcs, refLink = publicUrl.getOrElse(refLink), state = if (stateFinal.isEmpty) "Centre" else stateFinal, jobType = jobType,
      startDate = startDate, endDate = endDate, processType = processType, tags = tags, sourceId = sourceId, status = status.getOrElse("Unknown"),
      stats = Some(stats), originalPdf = extras.map(_.originalPdf), publicUrl = publicUrl, educationTags = Some(educationTags), liked = Some(likes.contains(_key.getOrElse(""))))
  }
}

case class JobResult(status: Boolean, message: String, items: Seq[Job])

case class JobSummaryResult(status: Boolean, message: String, items: Seq[JobSummary], locations: List[String], educations: List[String], jobTypes: List[String])

object SiteUrl {
  implicit val jsonFormatSiteUrl: Format[SiteUrl] = Json.format[SiteUrl]
  def getJustDomain(url: String): String = {
    url
      .replaceAll("""https://""", "")
      .replaceAll("""http://""", "")
      .split("/")
      .toList
      .headOption
      .getOrElse("").replaceAll("""www.""","").trim
  }
}

object Keywords {
  implicit val jsonFormatKeywords: Format[Keywords] = Json.format[Keywords]
}

object SimpleJob {
  implicit val jsonFormatSimpleJob: Format[SimpleJob] = Json.format[SimpleJob]
}


object JobStats {
  implicit val jsonFormatJobStats: Format[JobStats] = Json.format[JobStats]

  val empty = JobStats()
}


object FullJob {
  implicit val jsonFormatFullJob: Format[FullJob] = Json.format[FullJob]
}

object JobExtra {
  implicit val jsonFormatJobExtra: Format[JobExtra] = Json.format[JobExtra]
}

object JobSummary {
  implicit val jsonFormatJobSummary: Format[JobSummary] = Json.format[JobSummary]
}

object JobSummaryResult {
  implicit val jsonFormatJobSummaryResult: Format[JobSummaryResult] = Json.format[JobSummaryResult]
}


object JobResult {
  implicit val jsonFormatJobResult: Format[JobResult] = Json.format[JobResult]
}

case class JobSession(id: Option[String] = None, _key: Option[String] = None, name: Option[String], query: String, source: String, error: Option[String], page: Option[Pagination], dates: DatesInfo) extends ID {
}

object JobSession {
  implicit val jsonFormatNameValueSession: Format[JobSession] = Json.format[JobSession]
}

object Job {
  implicit val jsonFormatJob: Format[Job] = Json.format[Job]

  val APP_BASE_DOMAIN = {
    val domain = System.getenv("APP_BASE_DOMAIN")
    if (domain == null) "sarkarijoblisting.com" else domain
  }

  def removeBaseDomain(url: String): String = url.replaceAll(APP_BASE_DOMAIN, "")

  def generateSearchUrl(url: String, tag: String): String = {
    val u = s"""${url}?search=$tag"""
    s"""<a href="$u">$tag</a>"""
  }
  def makeJob(fullJobKey: Option[String], datas: Seq[EducationTag], employer: Option[Employer], fileWithContent: Option[FileWithContent], sourceMD: SourceMetadata, simpleJob: Option[SimpleJob]): Option[Job] = {
    val originalPdf = sourceMD.url.getOrElse("")
    val company = employer.map(x => x.name).getOrElse(datas.filter(x => x.mainTag.equalsIgnoreCase("Employer")).headOption.map(_.name).getOrElse(""))
    val state = employer.map(x => x.extra.state)
    val txtContent = datas.filter(x => x.mainTag.equalsIgnoreCase("Content")).headOption.map(_.name).getOrElse("")
    val employmentTypes = datas.filter(x => x.mainTag.equalsIgnoreCase("Employement Type"))
    val jobType = employmentTypes.headOption.map(_.name)
    val dates = datas.filter(x => x.mainTag.equalsIgnoreCase("Dates"))
    //println(s"Dates: $dates, start: $startDate and end: $endDate")
    val startDate: Option[LocalDate] = if (simpleJob.isDefined && simpleJob.get.startDate.isDefined) simpleJob.get.startDate else dates.filter(dt => dt.level.equals("Start Date")).sortBy(s => s.priority).map(_.name).map(dt => DatesInfo.toLocalDate(dt, "")).flatten.headOption
    val endDate = if (simpleJob.isDefined && simpleJob.get.endDate.isDefined) simpleJob.get.endDate else dates.filter(dt => dt.level.equals("Last Date")).sortBy(s => s.priority).map(_.name).map(dt => DatesInfo.toLocalDate(dt, "")).flatten.reverse.headOption
    val year = startDate.map(dt => dt.getYear)
    val month = startDate.map(dt => dt.getMonth).getOrElse("")
    val yr = year.map(_.toString).getOrElse("")
    val prefixTitle = employer.map(e => e.website.replaceAll("""https://""", "").replaceAll("""https://""", ""))
    val title = if (simpleJob.isDefined) simpleJob.get.title else {
      company match {
        case "" =>
          val tmpFilename = fileWithContent.map(c => c.name).getOrElse("").split("""(https:?-?|http:?-?|www\.)""").headOption.getOrElse("")
          val name = fileWithContent.map(c => c.name).getOrElse("").replaceAll("""[-_\(\)]""", " ").replaceAll("""\.pdf""", "")
          val n = s"${company} - Recruitment $month $yr"
          //val n = s"${company} $tmpFilename - Recruitment $month $yr"
          if (prefixTitle.isDefined) n.replaceAll(prefixTitle.getOrElse(""), "").replaceAll("_", "") else n
        case _ =>
          val tmpFilename = fileWithContent.map(c => c.name).getOrElse("").split("""(https:?-?|http:?-?|www\.)""").headOption.getOrElse("")
          val name = fileWithContent.map(c => c.name).getOrElse("").replaceAll("""[-_\(\)]""", " ").replaceAll("""\.pdf""", "")
          val yr = year.map(_.toString).getOrElse("")
          val n = s"$company - Recruitment $month $yr"
          //val n = s"$company ${tmpFilename} - Recruitment $month $yr"
          if (prefixTitle.isDefined) n.replaceAll(prefixTitle.getOrElse(""), "").replaceAll("_", "") else n
      }
    }
    val educations = datas.filter(x => x.mainTag.equalsIgnoreCase("Education")).distinct
    val education = educations.map(_.name).distinct.sortWith((a, b) => a < b)
    val adv = if (simpleJob.isDefined) simpleJob.get.advNo else datas.filter(x => x.mainTag.equalsIgnoreCase("Adv")).headOption.map(_.name)
    val process = if (simpleJob.isDefined && !simpleJob.get.walkIn.isEmpty) Some(simpleJob.get.walkIn) else datas.filter(x => x.mainTag.equalsIgnoreCase("Process Type")).headOption.map(_.name)
    val dt = DatesInfo.getCurrentDatesInUTC()
    val possibleDates = {Seq(startDate) ++ Seq(endDate) ++ dates.map(_.name).map(dt => DatesInfo.toLocalDate(dt, ""))}.filter(_.isDefined).map(_.get).distinct
    val keywords = if (sourceMD.negativeMatches.isDefined || sourceMD.positiveMatches.isDefined) Some(Keywords(positive = List(sourceMD.positiveMatches.getOrElse("")), negative = List(sourceMD.negativeMatches.getOrElse("")))) else None
    val extra = JobExtra(simpleJob = simpleJob, keywords = keywords, originalPdf = originalPdf, originalLabel = sourceMD.title, website = employer.map(_.website).getOrElse(""), department = employer.map(_.extra.department.getOrElse("")).getOrElse(""), educations = educations, employer = employer, possibleDates = possibleDates, dates = dt)
    val job = Job(title = title, jobType = jobType, company = company, startDate = startDate, endDate = endDate, advPath = sourceMD.downloadPath,
      educations = education, refLink = "", publicUrl = sourceMD.publicUrl, state = state, processType = process, sourceId = fullJobKey, content = fileWithContent.map(c => c.copy(content = txtContent)), extras = Some(extra), fetchStatus = Some("active"))
    //println(s"Job title: $title and process = $process")
    Some(job)
  }
}

