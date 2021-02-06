package controllers

import java.time.{LocalDate, ZonedDateTime}
import java.util.UUID

import actors.DataLoaderActor
import akka.actor.{ActorSystem, Props}
import akka.stream.{ActorMaterializer, Materializer}
import controllers.helpers.UserAction
import daos.{EducationDAO, EmployerDAO, FullJobDAO, JobDAO, SiteAndPageDAO}
import javax.inject.{Inject, Singleton}
import org.webjars.play.WebJarsUtil
import play.api.i18n.{I18nSupport, Lang, Langs, Messages, MessagesImpl}
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.libs.mailer.MailerClient
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}
import services.{ArangoDbService, ElasticsearchService}
import shared.{DropdownValue, Experiment, FullJob, ID, Job, JobExtra, JobResult, JobStats, JobSummary, JobSummaryResult, KeyID, PageCondition, Pagination, Process, Run, SitePage, SuccessResponse}

import scala.concurrent.{Await, ExecutionContext, Future}
import io.circe.generic.auto._
import models.AuthToken
import play.api.Configuration
import play.api.libs.concurrent.Futures

import scala.concurrent.duration.Duration
import daos.JobSessionDAO
import shared.JobSession
import shared.DatesInfo

@Singleton
class JobController @Inject()(
                               system: ActorSystem,
                               components: ControllerComponents,
                               langs: Langs,
                               template: views.html.jobs,
                               jobCustomPage: views.html.state_center_education_page,
                               jobView: views.html.job,
                               backofficeJobsView: views.html.backoffice.jobs,
                               jobsWebApp: views.html.jobs_web_app,
                               assetsFinder: AssetsFinder,
                               config: Configuration,
                                   db: ArangoDbService,
                                    es: ElasticsearchService,
                                   mailerClient: MailerClient
                                 )(
                                   implicit
                                   webJarsUtil: WebJarsUtil,
                                   assets: AssetsFinder,
                                   ex: ExecutionContext,
                                   val userAction: UserAction
                                 ) extends MainController[Job](components, db, es, langs, config) {

  //val logger = play.api.Logger(getClass)
  //implicit val lang: Lang = langs.availables.head
  //implicit val messages: Messages = MessagesImpl(lang, messagesApi)
  val repo = new JobDAO(db,es)
  val sitePage = new SiteAndPageDAO(db,es)
  val jobSessionDAO = new JobSessionDAO(db, es)

  override def index = Action.async { implicit request: Request[AnyContent] =>
    //val ref = system.actorOf(Props(new DataLoaderActor(db,es)), s"data-loading-actor-from-api")
    //ref ! Experiment("")
    sitePage.findByKeyValue("siteName", "jobs").map(page => {
      println(s"Page found: $page")
      val total = 0
      val records = Seq.empty
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
      Ok(template(page.getOrElse(SitePage.default), assetsFinder, Seq.empty, Seq.empty, ""))
    })
  }


  def save = Action.async(parse.json) { implicit request =>
    val payload = request.body
    repo.fromJson(payload.toString()) match {
      case Right(obj) =>
        repo.save(obj).map(x => Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200))))
      case Left(error) =>
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }

  def parseDocument = Action.async { implicit request: Request[AnyContent] =>
    val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
    println(s"Load and parse name: $name")
    import shared.utils.Slug._
    val actorname = name.slug
    val cacheFolder = CACHE_FOLDER
    name.isEmpty match {
      case true =>
        Future.successful(Ok("No data provided"))
      case false =>
        val ref = system.actorOf(Props(new DataLoaderActor(db,es)), s"data-loading-actor-$actorname")
        ref ! Run(name)
        Future.successful(Ok)
    }
  }

  def parseAndLoadFromCache= Action.async { implicit request: Request[AnyContent] =>
    val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
    val path: String = app.utils.Utils.decodeQueryString(request.queryString.get("path").flatMap(_.headOption).getOrElse(""))
    println(s"Load and parse name: $name")
    import shared.utils.Slug._
    val actorname = name.slug
    //val materializer: Materializer  = ActorMaterializer.create(system);
    //val ref = new DataLoaderActor()(materializer, ex)
    val cacheFolder = CACHE_FOLDER
    name.isEmpty match {
      case true =>
        val ref = system.actorOf(Props(new DataLoaderActor(db,es)), s"data-loading-actor-from-cache")
        val path = s"${cacheFolder}pdf"
        ref ! Process(path)
        Future.successful(Ok)
      case false =>
        val ref = system.actorOf(Props(new DataLoaderActor(db,es)), s"data-loading-actor-$actorname")
        val path = s"${cacheFolder}pdf/${name}"
        ref ! Run(path)
        Future.successful(Ok)
    }
  }

  def helperJobs(attribute: String, query: String, status: String, pageNo: Int, offset: Int, email: String = "", condition:String = "", jobQueryList: Seq[(String, String)] = Seq.empty): Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = {
    val limit = APP_OFFSET
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = for {
      page <- {
        if (condition.isEmpty)
        sitePage.findByKeyValue("siteName", "jobs")
        else
          sitePage.findByKeyValue("condition", condition)
      }
      skills <- new EducationDAO(db, es).findByQuery(Seq.empty, 0, 9999)
      totals <- repo.findByQuery(Seq(("status", status)), 0, 99999).map(_.size.toLong)
      likes <- userDao.find(email).map(u => u.map(_.likes).getOrElse(List.empty))
      jobs <- if (query.isEmpty) {
        //val totals = repo.getTotalCount()
        repo.findByQuery(if (status.isEmpty) Seq.empty else Seq(("status", status)), dropNo, offset).map(xs => (totals, xs))
        //repo.findByQuery(if (status.isEmpty) Seq(("status","Live")) else Seq(("status", status)), dropNo, offset).map(xs => (totals.toLong, xs))
      } else {
        //val tokens = query.split(" ").map(_.trim)
        val queries: Seq[String] = if (!attribute.isEmpty) Seq(attribute, query) else Seq.empty
        //val queries: Seq[String] = if (!attribute.isEmpty) tokens.map(f => s"$attribute:$f") else Seq.empty
        val finalQ = makeProperSearchQuery(query, status)
        val xs = repo.search(es, attribute, finalQ, dropNo, limit, queries)
        xs
      }
    } yield {
      (page.getOrElse(SitePage.default), jobs._1, jobs._2.map(_.toJobSummary(likes)), skills.sortBy(_.name).map(_.name))
    }
    result
  }

  def jobsByState(query: String) = Action.async { implicit request: Request[AnyContent] =>
    //val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val status: String = app.utils.Utils.decodeQueryString(request.queryString.get("status").flatMap(_.headOption).getOrElse(""))
    val category = "state"
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = helperJobs("state", query, status, pageNo, offset)
    result.map(r => {
      val total = r._2
      val records = r._3
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
      println(s"Jobs: by category $category Pagination = $pagination with drop = $dropNo")
      val sitepage = r._1
      val dt = LocalDate.now()
      val year = dt.getYear
      val tags = List(s"$query Government jobs $year", s"Government jobs in $query", s"Latest Government jobs in $query", s"Latest $query government job notifications", s"Sarkari jobs in $query $year", s"$query Government job results $year")
      Ok(template(sitepage.copy(title = s"Latest $query Government Job Notifications", tags = tags), assetsFinder, records, Seq.empty, query, jobsByCategory = category, jobsByCategoryValue = query))
    })
  }

  def jobsByEducation(category: String, query: String) = Action.async { implicit request: Request[AnyContent] =>
    //val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val status: String = app.utils.Utils.decodeQueryString(request.queryString.get("status").flatMap(_.headOption).getOrElse(""))
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = helperJobs("", query, status, pageNo, offset)
    result.map(r => {
      val total = r._2
      val records = r._3
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
      println(s"Jobs: by category $category Pagination = $pagination with drop = $dropNo")
      val sitepage = r._1
      val dt = LocalDate.now()
      val month = dt.getMonthValue
      val year = dt.getYear
      val tags = List(s"$query Government jobs $year", s"Government jobs in $query", s"Latest Government jobs in $query", s"Latest $query government job notifications", s"Sarkari jobs in $query $year", s"$query Government job results $year", s"Latest PSU jobs of $query $year")
      Ok(template(sitepage.copy(title = s"Latest Vacancies for $query with Central / State Government  - $month $year", tags = tags), assetsFinder, records, Seq.empty, query, jobsByCategory = category, jobsByCategoryValue = query))
    })
  }

  def getJobsApi = Action.async { implicit request: Request[AnyContent] =>
    val email: String = app.utils.Utils.decodeQueryString(request.queryString.get("email").flatMap(_.headOption).getOrElse(""))
    val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val status: String = app.utils.Utils.decodeQueryString(request.queryString.get("status").flatMap(_.headOption).getOrElse("Live"))

    val limit = APP_OFFSET
    val dropNo = (pageNo - 1) * offset
    val jobTypes = List("Apprenticeship", "Contractual", "Permanent")
    val jobTypesS = jobTypes.map(s => s"""jobType:"$s"""").mkString(" OR ")
    val jobType = "jobType"
    val jobTypeCount = query.sliding(jobType.length).count(window => window == jobType)
    val finalQ = if (jobTypeCount == 3) {
      query.replaceAll(s"""$jobTypesS""","").replaceAll("""\(\)""","").trim
    } else query
    println(s"API: Jobs request with pageNo = $pageNo, offset = $offset, query = $query and final Q = $finalQ before final processing of Q with status = $status")
    val sessionF = jobSessionDAO.save(JobSession(name = Some("Job"), query = query, source = email, error = None, page = None, dates = DatesInfo.getCurrentDatesInUTC()))
    val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = helperJobs("", finalQ, status, pageNo, offset, email)
    result.map(r => {
      val actualCount = r._3.size
      val userPageNo = ((pageNo - 1)* offset) + actualCount
      val total = r._2
      val countMessage = if (userPageNo <= total) s"${userPageNo}/${total}" else s"$total/$total"
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
      println(s"API Jobs: Pagination = $pagination with drop = $dropNo, actual count = $actualCount and user page no = $userPageNo / $total")
      val response = JobSummaryResult(status = true, message = countMessage, items = r._3, locations = api.JobParser.CENTRE_WITH_STATE_NAMES, educations = r._4.toList.distinct, jobTypes = jobTypes)
      sessionF.map(session => {
        Await.result(jobSessionDAO.save(session.copy(error = None, page = Some(pagination), dates = session.dates.copy(updated = Some(ZonedDateTime.now())))), Duration.apply(15, java.util.concurrent.TimeUnit.SECONDS))
      })
      Ok(Json.toJson(response))
    })
  .recover({
    case e: Throwable =>
      e.printStackTrace()
      val error = app.utils.Utils.stackTrace(e)
      val skillsF = new EducationDAO(db, es).findByQuery(Seq.empty, 0, 9999)
      val r = skillsF.map(skills => {
        val eduxs = skills.sortBy(_.name).map(_.name)
        val response = JobSummaryResult(status = true, message = "", items = Seq.empty
          , locations = api.JobParser.CENTRE_WITH_STATE_NAMES, educations = eduxs.distinct.toList, jobTypes = jobTypes)
        Ok(Json.toJson(response))
      })
      sessionF.map(session => {
        Await.result(jobSessionDAO.save(session.copy(error = Some(error), dates = session.dates.copy(updated = Some(ZonedDateTime.now())))), Duration.apply(15, java.util.concurrent.TimeUnit.SECONDS))
      })
      Await.result(r, Duration.Inf)
  })
}

val WEB_APP_HTTP_URL = {
    val url = System.getenv("WEB_APP_HTTP_URL")
    if (url == null) "https://web.sarkarijoblisting.com/index.html" else url
  }

      
  def jobs = Action.async { implicit request: Request[AnyContent] =>
    val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val source: String = app.utils.Utils.decodeQueryString(request.queryString.get("source").flatMap(_.headOption).getOrElse(""))
    source match {
      case "web-app" =>
        //Future.successful(Ok(jobsWebApp(SitePage.default, assetsFinder)))
        Future.successful(Redirect(WEB_APP_HTTP_URL))
      case _ =>
        //val status: String = app.utils.Utils.decodeQueryString(request.queryString.get("status").flatMap(_.headOption).getOrElse("Live"))
        val status = "Live"
        val limit = APP_OFFSET
        val dropNo = (pageNo - 1) * offset
        val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = helperJobs("", query, status, pageNo, offset)
        result.map(r => {
          val total = r._2
          val records = r._3
          val actualCount = r._3.size
          val userPageNo = ((pageNo - 1)* offset) + actualCount
          val countMessage = if (userPageNo <= total) s"${userPageNo} out of ${total}" else s"$total out of $total"
          implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt, countMessage = countMessage)
          println(s"Jobs: Pagination = $pagination with drop = $dropNo, actual count = $actualCount and user page no = $userPageNo / $total with countMessage = $countMessage")
          Ok(template(r._1, assetsFinder, records, Seq.empty, query))
        })
    }
  }

  def jobById(key: String) = Action.async { implicit request: Request[AnyContent] =>
    val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val limit = APP_OFFSET
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Option[Job])] = for {
      page <- sitePage.findByKeyValue("siteName", "jobs")
      jobs <- repo.findByID(KeyID(_key = Some(key), id = None))
    } yield {
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      val total = r._2.size
      val records = r._2
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
      println(s"Pagination = $pagination with drop = $dropNo")
      Ok(jobView(r._1, assetsFinder, r._2, educationsSkills = Seq.empty))
    })
  }

  def search() = Action.async { implicit request: Request[AnyContent] =>
    val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val limit: Int = app.utils.Utils.decodeQueryString(request.queryString.get("limit").flatMap(_.headOption).getOrElse("50")).toInt
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Long, Seq[Job])] = for {
      page <- sitePage.findByKeyValue("siteName", "jobs")
      jobs <- {
        val tmpQ = query match {
          case "fetchStatus_Pending" => """fetchStatus:"Pending""""
          case "status_Live" => """status:"Live" NOT extras.employer.status:"Live""""
          case _ => query
        }
        val finalQ = makeProperSearchQuery(tmpQ, "Live")
        println(s"Homepage jobs Search Q before getting results from ES = $finalQ")
        repo.search(es, "", searchQuery = finalQ, dropNo, limit)
      }
    } yield {
      (page.getOrElse(SitePage.default), jobs._1, jobs._2)
    }
    result.map(r => {
      println(s"No of records matched: ${r._2}")
      val total = r._2
      val records = r._3
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
      println(s"Pagination = $pagination with drop = $dropNo")
      val output = JobResult(status = true, message = s"Total matches: ${total}", items = records)
      Ok(play.api.libs.json.Json.toJson(output))
    })
  }

  def bookmarkJob = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val key = (payload \ "id").asOpt[String]
      val bookmarkCount: Int = (payload \ "bookmark").as[Int]
      val result: Future[Option[Job]] = for {
        objs <- repo.findByID(KeyID(_key = key, id = None))
        saveresult <- {
          if (objs.isDefined) {
            val stats: Option[JobExtra] = for {
              e <- objs.get.extras
              s <- {
                val stats = e.stats.getOrElse(JobStats())
                println(s"Found stats: $stats")
                Some(e.copy(stats = Some(stats.copy(viewCount = stats.viewCount + bookmarkCount, clickCount = stats.clickCount + bookmarkCount))))
              }
            } yield s
            val obj = objs.get.copy(extras = stats)
            repo.save(obj).map(Some(_))
          } else {
            Future.successful(None)
          }
        }
      } yield {
        //println(s"Saved job: $saveresult")
        saveresult
      }
      result.map(r => {
        r match {
          case Some(act) =>
            Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
          case _ =>
            Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(404, message = "Sorry, Job information cannot be found")))
        }
      })
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(s"Bookmark error: $error")
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }

  def saveLikedJob = Action.async(parse.json) { implicit request =>
    println(s"Headers = ${request.headers}")
    withUserJsonF(user => {
    try {
      val payload = request.body
      println(s"Saving user job like with payload = $payload by user $user")
      val username = (payload \ "email").as[String]
      val jid = (payload \ "jobId").as[String]
      val title = (payload \ "title").as[String]
      println(s"Trying to login using username = $username, title = $title and Job id = $jid")
      if (!jid.isEmpty && !username.isEmpty) {
        val f = for {
          u <- userDao.find(username)
          saved <- {
            val likes: List[String] = u.map(_.likes).getOrElse(List.empty)
            if (u.isDefined) {
              val finalLikes = if (likes.contains(jid)) {likes.filterNot(_.equalsIgnoreCase(jid))} else {likes ++ List(jid)}
              userDao.save(u.get.copy(likes = finalLikes.distinct))
            } else {
              Future.failed(new Exception("User not found."))
            }
          }
        } yield saved
        f.map(s => {
          println(s"Like jobs updated for username = $username for job id = $jid")
          Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "")))
        })
      } else {
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Invalid Job found.", exception = Some(s"$payload")))))
      }
    } catch {
      case _ : Throwable =>
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Job save error has occurred.", exception = Some("JSON error")))))
    }
  })
  }

  def jobsByPageCondition(condition: String, prefix: String, title: String, suffix: String) = Action.async { implicit request: Request[AnyContent] =>
    val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
    val source: String = app.utils.Utils.decodeQueryString(request.queryString.get("source").flatMap(_.headOption).getOrElse(""))
    source match {
      case "web-app" =>
        //Future.successful(Ok(jobsWebApp(SitePage.default, assetsFinder)))
        Future.successful(Redirect(WEB_APP_HTTP_URL))
      case _ =>
        val jobQueryList: Seq[(String, String)] = condition match {
          case "state" =>
            Seq(("state", prefix))
          case "state-code" =>
            Seq(("state", prefix))
          case "center" =>
            Seq(("extras.employer.extra.stateType", prefix))
          case "location" =>
            Seq(("state", prefix))
          case "profession" =>
            Seq(("state", prefix))
          case "employer" =>
            Seq(("extras.employer.name", prefix))
          case "industry" =>
            Seq(("extras.employer.extra.industry", prefix))
          case "education" =>
            Seq(("educations", prefix))
          case "education-in-state" =>
            Seq(("state", prefix), ("educations", suffix))
          case "education-in-center" =>
            Seq(("state", prefix), ("educations", suffix))
          case "education-in-district" =>
            Seq(("state", prefix), ("educations", suffix))
          case "education-in-location" =>
            Seq(("extras.employer.extra.area", prefix), ("educations", suffix))
          case "profession-in-state" =>
            Seq(("state", prefix))
          case "profession-in-center" =>
            Seq(("state", prefix))
          case "profession-in-district" =>
            Seq(("state", prefix))
          case "profession-in-location" =>
            Seq(("state", prefix))
          case _ =>
            Seq(("state", prefix))
        }
        val status = "Live"
        val limit = APP_OFFSET
        val dropNo = (pageNo - 1) * offset
        val result: Future[(SitePage, Long, Seq[JobSummary], Seq[String])] = helperJobs("", query, status, pageNo, offset, email = "", condition = condition, jobQueryList = jobQueryList)
        result.map(r => {
          val total = r._2
          val records = r._3
          val actualCount = r._3.size
          val userPageNo = ((pageNo - 1)* offset) + actualCount
          val countMessage = if (userPageNo <= total) s"${userPageNo} out of ${total}" else s"$total out of $total"
          implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt, countMessage = countMessage)
          println(s"Jobs: Pagination = $pagination with drop = $dropNo, actual count = $actualCount and user page no = $userPageNo / $total with countMessage = $countMessage")
          Ok(jobCustomPage(r._1, assetsFinder, records, Seq.empty, query))
        })
    }
  }
}

