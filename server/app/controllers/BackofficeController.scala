package controllers

import java.time.{LocalDate, ZonedDateTime}
import java.util.Calendar
import java.util.concurrent.TimeUnit

import actors.DataLoaderActor
import akka.actor.{ActorSystem, Props}
import api.{JobParser, PartnerCrawler, SiteParserAndCrawler}
import app.utils.FileUtils
import apps.Application.ruledao
import controllers.helpers.UserAction
import daos.{EducationDAO, EmployerDAO, FullJobDAO, JobDAO, RuleDAO, SiteAndPageDAO, SiteCrawlerDAO}
import io.circe.generic.auto._
import javax.inject.{Inject, Singleton}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.webjars.play.WebJarsUtil
import play.api.Configuration
import play.api.i18n.Langs
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import play.api.libs.mailer.MailerClient
import play.api.mvc.{AnyContent, ControllerComponents, Request, RequestHeader, Result}
import play.twirl.api.Html
import services.{ArangoDbService, ElasticsearchService}
import shared._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@Singleton
class BackofficeController @Inject()(
                               system: ActorSystem,
                               components: ControllerComponents,
                               langs: Langs,
                               template: views.html.jobs,
                               errorView: views.html.error,
                               backofficeAdsView: views.html.backoffice.ads,
                               backofficeJobsView: views.html.backoffice.jobs,
                               backofficeRulesView: views.html.backoffice.rules,
                               backofficeSkillsView: views.html.backoffice.skills,
                               backofficeApiCallView: views.html.backoffice.calls,
                               boPageView: views.html.backoffice.pages,
                               backofficeEmployeeView: views.html.backoffice.employees,
                               assetsFinder: AssetsFinder,
                               config: Configuration,
                                   db: ArangoDbService,
                                   mailerClient: MailerClient,
                               es: ElasticsearchService
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
  val repo = new JobDAO(db, es)
  val sitePage = new SiteAndPageDAO(db, es)

  val myExecutionContext: ExecutionContext = system.dispatchers.lookup("portal-context")
    
   implicit val runtime = zio.Runtime.default

  override def index = Action.async { implicit request: Request[AnyContent] =>
    //DateHelper.getAllDates("web site www.aiimsexams.org from 01.08.2019 to 21.08.2019 upto 5:00 P.M. No documents including\\nthe Registration Slip of on-line application form is required to be sent, however, all the applicants are")
    // uses the AssetsFinder API
    sitePage.findByKeyValue("siteName", "jobs").map(page => {
      println(s"Page found: $page")
      val total = 0
      val records = Seq.empty
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
      Ok(backofficeJobsView(page.getOrElse(SitePage.default), assetsFinder, Seq.empty, Seq.empty))
    })
  }

  def priv() = Action { implicit request: Request[AnyContent] =>
    withUser(user => Ok("DONE"))
  }

  def privPlay() = withPlayUser { user =>
    Ok(s"User found = $user")
  }

  def saveSiteCrawler = Action.async(parse.json) { implicit request =>
    import shared.utils.Slug.slugify
    val crawlerDao = new SiteCrawlerDAO(db, es)
    val payload = request.body
    crawlerDao.fromJson(payload.toString()) match {
      case Right(obj) =>
        val name = slugify(obj.url)
        val maybeObj = Await.result(crawlerDao.findByKeyValue("name", name), Duration.Inf)
        if (maybeObj.isDefined) {
          //val details = SiteCrawlerDetail.fromSiteCrawler(obj)
          crawlerDao.save(obj.copy(name = name)).map(x => Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200))))
        } else {
          crawlerDao.save(obj.copy(name = name)).map(x => Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200))))
        }
      case Left(error) =>
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Site Save error", false, Some(error)))))
    }
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

  def helperSiteCrawelerToFullJob(site: SiteCrawler, parsing: Boolean, isReParsing: Boolean, fetchAllParams: Boolean): scala.concurrent.Future[Seq[FullJob]] = {
    val myExecutionContext: ExecutionContext = system.dispatchers.lookup("portal-context")
    val cacheFolder = CACHE_FOLDER
    try {
        val dao = new SiteCrawlerDAO(db, es)
        val fulljobDao = new FullJobDAO(db, es)
        val params = Seq(("url", site.url))
        val output: Future[Seq[FullJob]] = dao.findByQuery(params, 0, 99999).map(datas => {
            val files: List[SourceMetadata] = datas.map(s => s.result.map(ss => ss.paths.zipWithIndex.map(c => {
              val link = if (ss.links.size >= ss.paths.size) ss.links(c._2) else ""
              val s3path = if (ss.s3paths.size >= ss.s3paths.size) ss.s3paths(c._2) else ""
              val name = if (ss.names.size >= ss.names.size) ss.names(c._2) else ""
              val path = if (ss.paths.size >= ss.paths.size) ss.paths(c._2) else ""
              val label = if (ss.labels.size >= ss.paths.size) ss.labels(c._2) else ""
              val publicUrl = if (!s3path.isEmpty) Some(s3path) else Some(link)
              SourceMetadata(path = c._1, url = Some(link), sourceUrl = Some(ss.sourceUrl), title = Some(label), downloadPath = Some(path), s3Path = Some(s3path), publicUrl = publicUrl, negativeMatches = ss.negativeMatches, positiveMatches = ss.positiveMatches)
            })
            ).getOrElse(Seq.empty)
            ).flatten.toList.filter(s => !s.url.getOrElse("").isEmpty)
            import akka.pattern.ask
            implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
            val finalfiles = files.filter(s => !s.path.isEmpty)
            println(s"Got urls to be processed for FullJob = ${finalfiles.size} for url = ${site.url}")
            val dataxs:Seq[FullJob] = finalfiles.zipWithIndex.map(xs => {
              //Future {
                  val i = xs._2
                  val p = xs._1
                    val fulljobR = fulljobDao.findByKeyValue("source.url", p.url.getOrElse(""))
                    val fulljob = Await.result(fulljobR, Duration(60, TimeUnit.SECONDS))
                    val rid = fulljob.map(_._key.getOrElse(""))
                    if (fulljob.isDefined && !isReParsing) {
                      println(s"helperSiteCrawelerToFullJob: Path already processed for FullJob = ${p.path}")
                      fulljob.get
                    } else {
                      val pid = shared.utils.Slug.slugify(p.url.getOrElse(""))
                      val actorname = s"data-loading-actor-from-cache-${pid}-$i"
                      val ref = system.actorOf(Props(new DataLoaderActor(db, es)), actorname)
                      val source = p
                      println(s"Started actor $actorname for path = ${source.path}")
                      val r = ref ? RunWithSource(source, parsing, rid, fetchAllParams)
                      app.utils.Utils.writeLog("/tmp/cache_processed.txt", source.path, true)
                      val fulljob:FullJob = Await.result(r.mapTo[FullJob], scala.concurrent.duration.Duration.Inf)
                      println(s"Stopped actor $actorname for path = ${source.path} with result = $fulljob")
                      ref ! Stop
                      fulljob
                    }
              //}(myExecutionContext)
            })
            dataxs
          })
          output
        }catch {
          case e: Exception =>
          e.printStackTrace()
          Future.successful(Seq.empty[FullJob])
        }
  }

  def processSiteCrawlerAsAds = Action.async { implicit request: Request[AnyContent] =>
    val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
    val isOverride: Boolean = app.utils.Utils.decodeQueryString(request.queryString.get("override").flatMap(_.headOption).getOrElse("false")).toBoolean
    println(s"Load and parse specific site crawler = $name with override = $isOverride")
    import shared.utils.Slug._
    val actorname = name.slug
    //val materializer: Materializer  = ActorMaterializer.create(system);
    //val ref = new DataLoaderActor()(materializer, ex)
    val myExecutionContext: ExecutionContext = system.dispatchers.lookup("portal-context")
    val cacheFolder = CACHE_FOLDER
    name.isEmpty match {
      case true =>
        val dao = new SiteCrawlerDAO(db, es)
        val fulljobDao = new FullJobDAO(db, es)
        val params = if (name.isEmpty) Seq.empty else Seq(("url", name))
        val totalCount = dao.getTotalCount()
        val currentOffset = 500
        val pages = totalCount / currentOffset
        //val pages = 1
        var i = 0
        while (i < pages) {
          dao.findByQuery(params, i, currentOffset).map(datas => {
            val files: List[SourceMetadata] = datas.map(s => s.result.map(ss => ss.paths.zipWithIndex.map(c => {
              val link = if (ss.links.size >= ss.paths.size) ss.links(c._2) else ""
              SourceMetadata(path = c._1, url = Some(link), sourceUrl = Some(ss.sourceUrl))
            })
            ).getOrElse(Seq.empty)
            ).flatten.toList.filter(s => !s.url.getOrElse("").isEmpty)
            import akka.pattern.ask
            implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
            val path = s"${cacheFolder}pdf"
            files.grouped(500).zipWithIndex.foreach(xs => {
              Future {
                xs._1.zipWithIndex.map(p => {
                  val i = xs._2
                  if (!p._1.path.isEmpty) {
                    val fulljobR = fulljobDao.findByKeyValue("source.path", p._1.path)
                    val fulljob = Await.result(fulljobR, Duration(10, TimeUnit.SECONDS))
                    if (fulljob.isDefined) {
                      println(s"Path already processed for FullJob = ${p._1.path}")
                    } else {
                      val actorname = s"data-loading-actor-from-cache-${p._2}-$i"
                      val ref = system.actorOf(Props(new DataLoaderActor(db, es)), actorname)
                      val source = p._1
                      println(s"Started actor $actorname for path = ${source.path}")
                      ref ? ExecuteAndDie(source)
                      app.utils.Utils.writeLog("/tmp/cache_processed.txt", source.path, true)
                    }
                  }
                })
              }(myExecutionContext)
            })
          })
          i += 1
        }
        Future.successful(Ok)
      case false =>
        val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
        val path = s"${cacheFolder}pdf/${name}"
        ref ! Run(path)
        Future.successful(Ok)
    }
  }


  def parseAndLoadFromCache = Action.async { implicit request: Request[AnyContent] =>
    val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
    println(s"Load and parse name: $name")
    import shared.utils.Slug._
    val actorname = name.slug
    //val materializer: Materializer  = ActorMaterializer.create(system);
    //val ref = new DataLoaderActor()(materializer, ex)
    val myExecutionContext: ExecutionContext = system.dispatchers.lookup("portal-context")
    val cacheFolder = CACHE_FOLDER
    name.isEmpty match {
      case true =>
        import akka.pattern.ask
        implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
        val path = s"${cacheFolder}pdf"
        FileUtils.getFilenames(path).grouped(250).zipWithIndex.foreach(xs => {
          Future {
            xs._1.zipWithIndex.map(p => {
              val i = xs._2
              val actorname = s"data-loading-actor-from-cache-${p._2}-$i"
              val ref = system.actorOf(Props(new DataLoaderActor(db, es)), actorname)
              val source = SourceMetadata(path = p._1)
              println(s"Started actor $actorname")
              ref ? ExecuteAndDie(source)
              app.utils.Utils.writeLog("/Users/pawan/git/github/pawank/sarkari-portal/cache_processed.txt", p._1, true)
            })
          }(myExecutionContext)
        })
        Future.successful(Ok)
      case false =>
        val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
        val path = s"${cacheFolder}pdf/${name}"
        ref ! Run(path)
        Future.successful(Ok)
    }
  }

  def adsById(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val dropNo = (pageNo - 1) * offset
    val result: Future[(SitePage, Option[FullJob])] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new FullJobDAO(db, es).findByID(KeyID(_key = Some(id), id = None))
    } yield {
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      val total = r._2.size
      val records = r._2
      implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
      println(s"Pagination = $pagination with drop = $dropNo")
      val job = r._2
      if (!job.isDefined)
        Ok(Json.toJson(ErrorResponse(code = 404, message = "Not found")))
      else {
        getAdContent match {
          case Some("content") =>
            Ok(Json.toJson(SuccessResponse(message = job.get.content.map(c => c.content).getOrElse(""), code = 200)))
          case Some("refLink") =>
            val link = routes.SiteAndPageController.getFileByTagAndId("ads", job.get._key.getOrElse(""), job.get.content.map(_.name).getOrElse("")).absoluteURL()
            Ok(link)
          case _ =>
            Ok(Json.toJson(job.get))
        }
      }
    })
  }


  def ads = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val keyEmp = app.utils.Utils.decodeQueryString(request.queryString.get("key").flatMap(_.headOption).getOrElse(""))
      val result: Future[(SitePage, Long, Seq[FullJob])] = for {
        page <- sitePage.findByKeyValue("siteName", "ads")
        jobs <- if (searchQuery.isDefined) {
          new FullJobDAO(db, es).search(es, "", searchQuery.getOrElse(""), dropNo, offset)
        } else {
          val dao = new FullJobDAO(db, es)
          val totals = dao.getTotalCount()
          if (keyEmp.isEmpty) {
            dao.findByQuery(Seq.empty, dropNo, offset).map(xs => (totals.toLong, xs))
          } else {
            dao.findByID(KeyID(_key = Some(keyEmp), id = None)).map(xs => {
              xs match {
                case Some(emp) => (1L, Seq(emp))
                case _ => (0L, Seq.empty)
              }
            })
          }
        }
      } yield {
        //println(s"Ads: $jobs")
        (page.getOrElse(SitePage.default), jobs._1, jobs._2)
      }
      result.map(r => {
        val total = r._2
        val records = r._3
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
        println(s"Pagination = $pagination with drop = $dropNo")
        Ok(backofficeAdsView(r._1, assetsFinder, records))
      })
    })
  }

  def matchingRules = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val dao = new RuleDAO(db, es)
      val result: Future[(SitePage, Seq[MatchingRule], Seq[Education])] = for {
        page <- sitePage.findByKeyValue("siteName", "rules")
        skills <- new EducationDAO(db, es).findByQuery(Seq.empty, 0, 99999)
        jobs <- dao.findByQuery(Seq.empty, 0, 99999)
      } yield {
        (page.getOrElse(SitePage.default), jobs, skills)
      }
      result.map(r => {
        val total = r._2.size
        val records = r._2
        val rules = makeNonEmptyRecordsAndSort(records.filter(m => !m.name.equalsIgnoreCase("Specialisation") && !m.name.equalsIgnoreCase("Level")).map(m => {
          m.name
        }).toList)
        val specialisation = makeNonEmptyRecordsAndSort(records.filter(m => m.name.equalsIgnoreCase("Specialisation")).map(m => {
          m.matches
        }).flatten.toList)
        val levels = makeNonEmptyRecordsAndSort(records.filter(m => m.name.equalsIgnoreCase("Level")).map(m => {
          m.matches
        }).flatten.toList)
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
        //println(s"Pagination = $pagination with drop = $dropNo")
        //val names = rules.map(x => NameValue(x, x)).distinct.sortBy(a => a.name)
        val names = rules.map(x => NameValue(x, x)).distinct.sortBy(a => a.name) ++ Seq(NameValue("Specialisation", "Specialisation"))
        Ok(backofficeRulesView(r._1, assetsFinder, records, names, specialisation, levels))
      })
    })
  }

  def showPage(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val source: String = app.utils.Utils.decodeQueryString(request.queryString.get("source").flatMap(_.headOption).getOrElse(""))
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val dao = new RuleDAO(db, es)
      val crawlerDao = new SiteAndPageDAO(db, es)
      val result: Future[(SitePage, Seq[SitePage])] = for {
        page <- {
          if (id.isEmpty || id.equalsIgnoreCase("0"))
          sitePage.findByKeyValue("siteName", "backoffice pages")
          else
            sitePage.findByID(KeyID(_key = Some(id)))
        }
        skills <- {
          crawlerDao.findByQuery(Seq.empty, 0, 50, sortingKey = "dates.created DESC")
        }
      } yield {
        (page.getOrElse(SitePage.default), skills)
      }
      result.map(r => {
        val total = r._2.size
        val records = r._2
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
        Ok(boPageView(r._1, assetsFinder, r._1, records, PageCondition.conditions))
      })
    })
  }


  def savePage = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val action: Option[String] = (payload \ "action").asOpt[String]
      val title: String = (payload \ "title").as[String]
      val dao = new SiteAndPageDAO(db, es)
      payload.validate[SitePage] match {
        case JsSuccess(obj, path) =>
          val result: Future[Option[SitePage]] = for {
            objs <- dao.findByKeyValue("title", title)
            saveresult <- {
              if (obj.url.isEmpty) {
                Future.successful(None)
              } else {
                if (objs.isDefined) {
                  val updatedOne = obj.copy(id = objs.get.id, _key = objs.get._key)
                  dao.save(updatedOne).map(Some(_))
                } else {
                  dao.save(obj).map(Some(_))
                }
              }
            }
          } yield {
            saveresult
          }
          result.map(r => {
            action match {
              case Some(act) =>
                if (r.isDefined && !r.get.url.isEmpty)
                  Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
                else
                  Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error", false, None)))
              case _ =>
                Redirect(routes.BackofficeController.showCallApi())
            }
          })
        case JsError(errors) =>
          println(s"ERRORS in saving crawler for job fetch = ${errors.toString()}")
          Future.successful(Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error", false, Some(errors.toString())))))
      }
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(error)
        Future.successful(Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error", false, Some(error)))))
    }
  }

  def showCallApi = Action.async { implicit request: Request[AnyContent] =>
    val source: String = app.utils.Utils.decodeQueryString(request.queryString.get("source").flatMap(_.headOption).getOrElse(""))
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val dao = new RuleDAO(db, es)
      val crawlerDao = new SiteCrawlerDAO(db, es)
      val result: Future[(SitePage, Seq[MatchingRule], Seq[SiteCrawler])] = for {
        page <- sitePage.findByKeyValue("siteName", "fetch jobs")
        skills <- {
          val params = source match {
            case "team" => Seq(("source", source))
            case "engine" => Seq(("source", ""))
            case _ => Seq(("source", "team"))
          }
          //crawlerDao.findByQuery(params, 0, 99999)
          crawlerDao.findByQuery(params, 0, 99999, sortingKey = "priority DESC")
          //crawlerDao.findByQuery(params, 0, 50, sortingKey = "result.dates.created DESC")
        }
        rules <- dao.findByQuery(Seq.empty, 0, 99999)
      } yield {
        val sites = skills.filter(s => s.result.isDefined && s.result.get.completed.isDefined).sortWith((a, b) => {
          val r1 = a.result.map(_.completed.getOrElse(ZonedDateTime.now())).getOrElse(ZonedDateTime.now())
          val r2 = b.result.map(_.completed.getOrElse(ZonedDateTime.now())).getOrElse(ZonedDateTime.now())
          r1.toInstant.toEpochMilli > r2.toInstant.toEpochMilli
        })
        (page.getOrElse(SitePage.default), rules, sites.take(50))
      }
      result.map(r => {
        val total = r._3.size
        val rules = r._2
        val records = r._3
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
        //println(s"Pagination = $pagination with drop = $dropNo")
        //val names = rules.map(x => NameValue(x, x)).distinct.sortBy(a => a.name)
        val namesFinal = Seq("--", "team", "engine").map(x => NameValue(x, x)).distinct.sortBy(a => a.name)
        Ok(backofficeApiCallView(r._1, assetsFinder, records, namesFinal))
      })
    })
  }

  def callApiForFetch = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val action: Option[String] = (payload \ "action").asOpt[String]
      val name : String = (payload \ "name").as[String]
      val url: String = (payload \ "url").as[String]
      val priority: Int = (payload \ "priority").as[Int]
      val description: String = (payload \ "description").as[String]
      val dao = new SiteCrawlerDAO(db, es)
      payload.validate[SiteCrawler] match {
        case JsSuccess(obj, path) =>
          val result: Future[Option[SiteCrawler]] = for {
            objs <- dao.findByKeyValue("url", url)
            saveresult <- {
              if (obj.url.isEmpty) {
                Future.successful(None)
              } else {
                val finalName = obj.url.split("/").reverse.head
                if (objs.isDefined) {
                  println(s"EXISTING site = ${objs.get}")
                  val oldResult = objs.get.result.getOrElse(SiteCrawlerResult.getSiteCrawlerResult(finalName, obj.url)).copy(started = Some(shared.DatesInfo.getCurrentZonedDateUTC()), dates = DatesInfo.getCurrentDatesInUTC())
                  val c = objs.get.copy(priority = obj.priority, source = obj.source, name = finalName, originalUrl = Some(obj.url), result = Some(oldResult))
                  dao.save(c).map(Some(_))
                } else {
                  val oldResult = SiteCrawlerResult.getSiteCrawlerResult(finalName, obj.url).copy(started = Some(shared.DatesInfo.getCurrentZonedDateUTC()), dates = DatesInfo.getCurrentDatesInUTC())
                  dao.save(obj.copy(name = finalName, originalUrl = Some(obj.url), result = Some(oldResult))).map(Some(_))
                }
              }
            }
          } yield {
            Future {
              if (saveresult.isDefined) {
                val updatedOutput = saveresult.get.result
                println(s"Started fetching data for file from $saveresult with start time = ${updatedOutput.map(_.dates.created)}")
                val fetchAllParams = if (description.contains("competitors")) true else false
                val output = fetchSingleJob(concurrencyNo = 1, offsetNo = 1, pageNo = 0, priority = priority, fetchAllParams = fetchAllParams, name, obj.url, linksToFetch = obj.url, isFetchSingleOnly = true)
                val alreadyDone = output.contains("Fetch Status: ")
                val msg = if (alreadyDone) output.split("""\^""")(0) else ""
                val status = updatedOutput.map(s => if (s.s3paths.isEmpty || output.isEmpty() || alreadyDone) {if (alreadyDone) msg else "Failed"} else "Success")
                val endTime = shared.DatesInfo.getCurrentZonedDateUTC()
                println(s"Done fetching data for file from $saveresult with status = $status with end time = ${endTime}")
                dao.save(saveresult.get.copy(output = Some(output), result = updatedOutput.map(_.copy(dates = updatedOutput.get.dates.copy(created = endTime), status = status, completed = Some(endTime)))))
                //dao.save(saveresult.get.copy(output = Some(output), result = updatedOutput.map(_.copy(status = status, completed = Some(endTime)))))
                //Await.result(dao.save(r.get.copy(output = Some(output))), Duration.Inf)
              }
            }
            saveresult
          }
          result.map(r => {
            action match {
              case Some(act) =>
                if (r.isDefined && !r.get.url.isEmpty)
                  Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
                else
                Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error: No URL found", false, None)))
              case _ =>
                Redirect(routes.BackofficeController.showCallApi())
            }
          })
        case JsError(errors) =>
          println(s"ERRORS in saving crawler for job fetch = ${errors.toString()}")
          Future.successful(Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error", false, Some(errors.toString())))))
      }
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(error)
        Future.successful(Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(500, "Save error", false, Some(error)))))
    }
  }

  def educationSkills = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val dao = new RuleDAO(db, es)
      val result: Future[(SitePage, Seq[MatchingRule], Seq[Education])] = for {
        page <- sitePage.findByKeyValue("siteName", "backoffice skills")
        skills <- new EducationDAO(db, es).findByQuery(Seq.empty, 0, 99999)
        rules <- dao.findByQuery(Seq.empty, 0, 99999)
      } yield {
        (page.getOrElse(SitePage.default), rules, skills)
      }
      result.map(r => {
        val total = r._3.size
        val rules = r._2
        val records = r._3.map(e => {
          e.toMatchingRule()
        })
        val names = makeNonEmptyRecordsAndSort(records.filter(m => !m.name.equalsIgnoreCase("Specialisation") && !m.name.equalsIgnoreCase("Level")).map(m => {
          m.name
        }).toList)
        val skills = makeNonEmptyRecordsAndSort(rules.filter(m => m.name.equalsIgnoreCase("Specialisation")).map(m => {
          m.matches
        }).flatten.toList)
        val levels = makeNonEmptyRecordsAndSort(rules.filter(m => m.name.equalsIgnoreCase("Level")).map(m => {
          m.matches
        }).flatten.toList)
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
        //println(s"Pagination = $pagination with drop = $dropNo")
        //val names = rules.map(x => NameValue(x, x)).distinct.sortBy(a => a.name)
        val namesFinal = names.map(x => NameValue(x, x)).distinct.sortBy(a => a.name) ++ Seq(NameValue("Specialisation", "Specialisation"))
        Ok(backofficeSkillsView(r._1, assetsFinder, records, namesFinal, skills, levels))
      })
    })
  }


  def saveEducationSkills = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val id: Option[String] = (payload \ "id").asOpt[String]
      val action: Option[String] = (payload \ "action").asOpt[String]
      val title: String = (payload \ "title").as[String]
      val tag: String = (payload \ "tag").as[String]
      val category: String = (payload \ "category").as[String]
      val matches: List[String] = (payload \ "matches").as[List[String]]
      val tags: List[String] = (payload \ "tags").as[List[String]]
      val level: String = (payload \ "level").as[String]
      val description: String = (payload \ "description").as[String]
      val slug: String = (payload \ "slug").as[String]
      val key = if (id.isDefined) "_key" else "name"
      val value = {
        val tmp = id.getOrElse(title)
        if (tmp.isEmpty) tag else tmp
      }
      val dao = new EducationDAO(db, es)
      println(s"Title = $title / tag = $tag and matches: ${matches} / $tags")
      val result: Future[Education] = for {
        objs <- dao.findByKeyValue(key, value)
        saveresult <- {
          val finaltags = makeNonEmptyRecordsAndSort(matches ++ tags)
          val finalName = if (title.isEmpty) tag else title
          //println(s"Final tags in matching rule: $finaltags")
          if (objs.isDefined) {
            val obj = objs.get.copy(name = finalName, code = finalName.toLowerCase, tags = finaltags, category = Some(category), level = if (level.isEmpty) None else Some(level), description = Some(description))
            dao.save(obj)
          } else {
            val obj = Education(code = finalName.toLowerCase, name = finalName, tags = finaltags, category = Some(category), level = if (level.isEmpty) None else Some(level), description = Some(description), id = None, _key = None)
            dao.save(obj)
          }
        }
      } yield {
        //println(s"Got job: $jobs")
        saveresult
      }
      result.map(r => {
        action match {
          case Some(act) =>
            Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
          case _ =>
            Redirect(routes.BackofficeController.educationSkills())
        }
      })
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }

  def saveMatchingRule = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val id: Option[String] = (payload \ "id").asOpt[String]
      val action: Option[String] = (payload \ "action").asOpt[String]
      val title: String = (payload \ "title").as[String]
      val tag: String = (payload \ "tag").as[String]
      val category: String = (payload \ "category").as[String]
      val matches: List[String] = (payload \ "matches").as[List[String]]
      val tags: List[String] = (payload \ "tags").as[List[String]]
      val level: String = (payload \ "level").as[String]
      val description: String = (payload \ "description").as[String]
      val slug: String = (payload \ "slug").as[String]
      val key = if (id.isDefined) "_key" else "name"
      val value = {
        val tmp = id.getOrElse(title)
        if (tmp.isEmpty) tag else tmp
      }
      println(s"Title = $title / tag = $tag and matches: ${matches} / $tags")
      val result: Future[MatchingRule] = for {
        objs <- new RuleDAO(db, es).findByKeyValue(key, value)
        saveresult <- {
          val finaltags = makeNonEmptyRecordsAndSort(matches ++ tags)
          val finalName = if (title.isEmpty) tag else title
          //println(s"Final tags in matching rule: $finaltags")
          if (objs.isDefined) {
            val obj = objs.get.copy(name = finalName, rule = finalName.toLowerCase, matches = finaltags, category = category, level = if (level.isEmpty) None else Some(level), slug = Some(slug), description = Some(description))
            new RuleDAO(db, es).save(obj)
          } else {
            val obj = MatchingRule(rule = finalName.toLowerCase, name = finalName, matches = finaltags, matchType = "Education", category = category, level = if (level.isEmpty) None else Some(level), slug = Some(slug), description = Some(description), id = None, _key = None)
            new RuleDAO(db, es).save(obj)
          }
        }
      } yield {
        //println(s"Got job: $jobs")
        saveresult
      }
      result.map(r => {
        action match {
          case Some(act) =>
            Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
          case _ =>
            Redirect(routes.BackofficeController.matchingRules())
        }
      })
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }

  def employers = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val keyEmp = app.utils.Utils.decodeQueryString(request.queryString.get("key").flatMap(_.headOption).getOrElse(""))
      val matching = app.utils.Utils.decodeQueryString(request.queryString.get("matching").flatMap(_.headOption).getOrElse(""))
      println(s"Employers page = ${pageNo} and offset = $offset and QA key = $keyEmp and matching = $matching")
      val dropNo = (pageNo - 1) * offset
      val result: Future[(SitePage, Long, Seq[Employer])] = for {
        page <- sitePage.findByKeyValue("siteName", "employers")
        jobs <- {
          val empsDao = new EmployerDAO(db, es)
          if (!searchQuery.isDefined) {
            val totals = empsDao.getTotalCount()
            if (keyEmp.isEmpty) {
              empsDao.findByQuery(Seq.empty, dropNo, offset).map(xs => (totals.toLong, xs))
            } else {
              empsDao.findByID(KeyID(_key = Some(keyEmp), id = None)).map(xs => {
                xs match {
                  case Some(emp) => (1L, Seq(emp))
                  case _ => (0L, Seq.empty)
                }
              })
              //empsDao.findByQuery(Seq(("_key", keyEmp)), dropNo, offset).map(xs => (totals.toLong, xs))
            }
          } else {
            val sq = searchQuery.getOrElse("")
            val q = makeProperSearchQuery(sq, "")
            empsDao.search(es, "", q, dropNo, offset)
          }
        }
      } yield {
        (page.getOrElse(SitePage.default), jobs._1, jobs._2.sortWith((a, b) => a._key.getOrElse("") < b._key.getOrElse("")))
      }
      result.map(r => {
        val total = r._2
        val records = r._3.sortWith((a, b) => a._key.getOrElse("") < b._key.getOrElse("")).map(emp => {
          if (emp.extra.stateType.equalsIgnoreCase("Center") || emp.extra.stateType.equalsIgnoreCase("Centre")) {
            if (emp.extra.state.isEmpty) emp.copy(extra = emp.extra.copy(state = "Central Government")) else emp
          } else emp
        })
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
        println(s"Pagination = $pagination with drop = $dropNo")
        val names = records.map(x => NameValue(x.code, x.name)).distinct.sortBy(a => a.name)
        Ok(backofficeEmployeeView(r._1, assetsFinder, records, names, if (keyEmp.isEmpty) None else Some(keyEmp)))
      })
    })
  }

  def getEmployersByQuery(query: String) = Action.async { implicit request =>
    withAdminUserF(user => {
      try {
        val result: Future[DropdownResponse] = for {
          objs <- new EmployerDAO(db, es).searchByQuery(Seq(("name", query), ("code", query)), 0, 99)
        } yield {
          DropdownResponse(success = true, results = objs.map(emp => DropdownValue(name = s"${emp.name} ${emp.website}", value = emp._key.getOrElse(""), text = s"${emp.name} [${emp.website}]")))
        }
        result.map(r => {
          //Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
          Ok(play.api.libs.json.Json.toJson(r))
        })
      } catch {
        case e: Exception =>
          val error = app.utils.Utils.stackTrace(e)
          Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
      }
    })
  }

  def saveCrawlingSitesFromFetchingUrls(linksToFetch: List[String]) = {
    val crawlerDao = new SiteCrawlerDAO(db, es)
    val linksTobefetched: List[SiteCrawler] = {
      val urlList: List[String] = linksToFetch.filter(!_.isEmpty)
      println(s"URL to be fetched and parsed = $urlList")
      val crawlingSites = urlList.map(link => {
        val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
        SiteCrawler(name = name, url = link, domain = Some(SiteUrl.getJustDomain(link)), ignore = Some("no"), priority = 500, status = "", result = None, originalUrl = Some(link), siteType = Some("sites"))
      })
      crawlingSites.map(site => {
        val oldSites = Await.result(crawlerDao.findByQuery(Seq(("url", site.originalUrl.getOrElse(site.url))), 0, 999999), Duration.Inf)
        if (oldSites.isEmpty) Await.result(crawlerDao.save(site), Duration.Inf) else Await.result(crawlerDao.save(oldSites.head.copy(priority = 500, siteType = Some("sites"))), Duration.Inf)
      })
    }
    println(s"No of SITES to be processed with user input = ${linksTobefetched.size}")
  }

  def saveEmployers = Action.async(parse.json) { implicit request =>
    val payload = request.body
    try {
      println(s"Payload: $payload")
      val id: Option[String] = (payload \ "id").asOpt[String]
      val action: Option[String] = (payload \ "action").asOpt[String]
      val title: String = (payload \ "name").as[String]
      val code: String = (payload \ "code").as[String]
      val stateType: String = (payload \ "stateType").as[String]
      val state: String = (payload \ "state").as[String]
      val website: String = (payload \ "website").as[String].toLowerCase
      val domain = Employer.getWebsite(website)
      val industry: String = (payload \ "industry").as[String]
      val department: String = (payload \ "department").as[String]
      val links: List[String] = (payload \ "links").as[List[String]].map(_.trim)
      val key = if (id.isDefined && !id.getOrElse("").isEmpty) "_key" else "name"
      val value = if (id.isDefined && !id.getOrElse("").isEmpty) id.getOrElse("") else title
      val hash = s"${title.toLowerCase} - $website"
      println(s"id = $key, Title = $title and matches: ${code} and $website and # $hash and crawling links = $links")
      if ((title.isEmpty) || website.isEmpty) {
        val error = s"Please verify name and website"
        Future.successful(Ok(play.api.libs.json.Json.toJson(shared.ErrorResponse(400, error, false, Some(error)))))
      } else {
        val result: Future[Employer] = for {
          objs <- {
            if (id.isDefined && !id.getOrElse("").isEmpty) {
              new EmployerDAO(db, es).findByID(KeyID(_key = id))
            } else {
              new EmployerDAO(db, es).findByQuery(Seq(("website", website), ("domain",domain)), 0, 99999, conditionType = "OR").map(qs => {
                qs.headOption
              })
            }
          }
          saveresult <- {
            if (objs.isDefined) {
              val extra = objs.get.extra.copy(links = if (links.isEmpty) None else Some(links), stateType = stateType, state = state, department = if (!department.isEmpty) Some(department) else None, industry = if (!industry.isEmpty) Some(industry) else None)
              val obj = objs.get.copy(domain = Some(domain), hash = Some(hash), code = code, name = title, website = website, extra = extra)
              new EmployerDAO(db, es).save(obj)
            } else {
              val extra = EmployerExtra(links = if (links.isEmpty) None else Some(links), category = "", stateType = stateType, state = state, area = "", department = if (!department.isEmpty) Some(department) else None, industry = if (!industry.isEmpty) Some(industry) else None)
              val obj = Employer(id = None, _key = None, domain = Some(domain), hash = Some(hash), name = title, code = code, website = website, extra = extra)
              new EmployerDAO(db, es).save(obj)
            }
          }
        } yield {
          val linksTobeSaved = saveresult.extra.links.getOrElse(List.empty)
          if (!linksTobeSaved.isEmpty)
            saveCrawlingSitesFromFetchingUrls(linksTobeSaved)
          saveresult
        }
        result.map(r => {
          action match {
            case Some(act) =>
              println(s"Employer action = $act done")
              Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
            case _ =>
              //Redirect(routes.BackofficeController.employers())
              println(s"Employer save done")
              Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
          }
        })
      }
    } catch {
      case e: Exception =>
        val error = app.utils.Utils.stackTrace(e)
        println(s"ERROR: Employer update: $error")
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(400, "Save error", false, Some(error)))))
    }
  }

  def deleteSiteCrawler(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new SiteCrawlerDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"Deleted SiteCrawler: $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record(s) has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record(s) cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }

  def deletePage(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new SiteCrawlerDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"Deleted page : $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record(s) has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record(s) cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }


  def deleteSkills(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new EducationDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"Deleted Education: $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record(s) has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record(s) cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }


  def deleteRule(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new RuleDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"Deleted rule: $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }

  def deleteEmployer(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new EmployerDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"Deleted employer: $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }


  def updateAdsStatus(id: String, status: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Option[FullJob])] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new FullJobDAO(db, es).findByID(KeyID(_key = Some(id)))
      saveresult <- {
        if (jobs.isDefined)
          new FullJobDAO(db, es).save(jobs.get.copy(status = Some(status), dates = jobs.get.dates.copy(updated = Some(ZonedDateTime.now())))).map(Some(_))
        else Future.successful(None)
      }
    } yield {
      //println(s"Got job: $jobs")
      (page.getOrElse(SitePage.default), saveresult)
    }
    result.map(r => {
      if (r._2.isDefined) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Status has been updated"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Status cannot be updated", exception = Some("ID cannot be found."))))
    })
  }


  def parseFullJob(obj: FullJob, employers: Seq[Employer], matches: Seq[MatchingRule], crawler: Option[SiteCrawler] = None): Future[Either[String, Seq[Job]]] = {
    val name = obj._key.getOrElse("")
    import shared.utils.Slug._
    val actorname = name.slug
    import akka.pattern.ask
    implicit val timeout: akka.util.Timeout = Duration.apply(900, TimeUnit.SECONDS)
    //val ref = system.actorOf(Props(new DataLoaderActor(db,es)), s"data-loading-actor-$actorname")
    //val resultF: Future[Tuple2[Seq[EducationTag], Seq[Employer]]] = {ref ? ParseFullJob(obj, employers, matches)}.mapTo[Tuple2[Seq[EducationTag], Seq[Employer]]]
    //if (ref != null) {
    //  ref ! Stop
    //}
    val resultFF: Future[Either[String, Seq[Job]]] = Future {
      try {
        implicit val timeout: akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
        val ref = new JobParser(obj, employers, matches)
        val simpleJob = PartnerCrawler.siteCrawlerToEnhancedSimpleJon(obj.source.url.getOrElse(""), crawler)
        println(s"Converting ${obj.source.url} to new Job")
        val xs: Tuple2[Seq[EducationTag], Seq[Employer]] = {
          val employerDAO = new EmployerDAO(db, es)
          val foundLinks: Seq[SiteUrl] = simpleJob.map(_.links.getOrElse(Seq.empty)).getOrElse(Seq.empty).filter(e => e.whatFor.contains("website") || e.whatFor.contains("apply"))
          val empUrl = foundLinks.headOption.map(_.url).getOrElse("")
          val empDomain = SiteUrl.getJustDomain(empUrl)
          val maybeEmp = Await.result(employerDAO.findByQuery(Seq(("domain", empDomain)), 0, 1), Duration.Inf)
          val r = ref.parse()
          if (!maybeEmp.isEmpty) (r._1, maybeEmp) else r
        }
        //println(s"Dates: $dates, start: $startDate and end: $endDate")
        val job = Job.makeJob(obj._key, xs._1, xs._2.reverse.headOption, obj.content, obj.source, simpleJob)
        //println(s"JOB: $job")
        if (job.isDefined) {
          //val link = routes.SiteAndPageController.getFileByTagAndId("ads", obj._key.getOrElse(""), obj.content.map(_.name).getOrElse("")).absoluteURL()
          val datas:Seq[Job] = if (!xs._2.isEmpty) {
            xs._2.map(emp => {
            Job.makeJob(obj._key, xs._1, Some(emp), obj.content, obj.source, simpleJob)
          }).filter(_.isDefined).map(_.get)
          } else Seq(job.get)
          Right(datas)
        } else {
          println(s"ERROR: $xs in processing Ad = ${obj._key} in parseFullJob")
          Left(xs.toString())
        }
      } catch {
        case e: Exception =>
          val error = app.utils.Utils.stackTrace(e)
          println(s"ERROR: Exception $error in processing Ad = ${obj._key} in parseFullJob")
          Left(error)
      }
    }(myExecutionContext)
    resultFF
  }

  val isOverrideExistingGooglePdfFetchedDocs = APP_OVERRIDE_GOOGLE_PDF_FETCH
 
   def fixPdfBaseUrl(link: String): String = {
                link.replaceAll("""http://www.sarkarijoblisting.com""", APP_API_BASE_URL).replaceAll("""http://admin.sarkarijoblisting.com""", APP_API_BASE_URL).replaceAll("""http://localhost:9987""", APP_API_BASE_URL).replaceAll("""http://localhost:9988""", APP_API_BASE_URL).replaceAll("""http://localhost:9000""", APP_API_BASE_URL)

}

  def getOriginalAdLink(jd: FullJob): FullJob = {
    val jp = new JobParser(jd, Seq.empty, Seq.empty)
    //val jp = jd.content.map(c => c.content).getOrElse("")
    //val h = Html(jp)
    //val doc: Document = Jsoup.parse(jp)
    //val originalContent =  doc.body().wholeText()
    //val originalContent =  doc.head().wholeText() + " " + doc.body().wholeText()
    val originalContent = jp.originalContentText
    val q = originalContent.take(api.GoogleSearch.TEXT_LIMIT)
    //val gresult = if ((!isOverrideExistingGooglePdfFetchedDocs) && jd.googleSearchResult.isDefined) jd.googleSearchResult else api.GoogleSearch.searchAndDownload(APP_GOOGLE_SEARCH_API_KEYS, CACHE_FOLDER, q)
    val gresult = api.GoogleSearch.searchAndDownload(APP_GOOGLE_SEARCH_API_KEYS, CACHE_FOLDER, q)
    //println(s"Found google result = $gresult for full JD = ${jd._key}")
    var originalLink: Option[String] = None
    val r = if (gresult.isDefined) {
      val g = gresult.get
      val matching = api.GoogleSearch.performMatchingOfResults(g, originalContent)
      println(s"Google result pdf and original content matching output = $matching for paths = ${g.paths}")
      matching match {
        case Some(data) =>
          if (data._3) {
            originalLink = Some(data._2)
          }
          println(s"Found new original link = $originalLink for full JD = ${jd._key}")
          Some(g.copy(notificationLink = data._1, notificationPdf = data._2, ok = data._3))
        case _ =>
          gresult
      }
    } else None
    println(s"Original link found = $originalLink for key = ${jd._key}")
    jd.copy(originalLink = originalLink, googleSearchResult = r)
  }

  def downloadJobFile = Action.async { implicit request: Request[AnyContent] =>
    val raw = request.body.asJson
    val id = raw match {
      case Some(v) => (v \ "id").as[String]
      case _ => ""
    }
    println(s"Downloading PDF for Job with ID = ${id}")
    val result: Future[Either[String, Job]] = for {
      mayjobs <- repo.findByID(KeyID(_key = Some(id)))
      runResults <- {
        if (mayjobs.isDefined) {
          val jd = mayjobs.get
          println(s"Parsing Job with ID = ${jd._key}")
          val url = jd.extras.map(e => e.originalPdf).getOrElse("")
          val (filePath: String, finalUrl: String) = {
            val (a: String, b: String) = SiteParserAndCrawler.downloadAndCacheUrlToFile(url, jd.content.map(c => {
              val path = if (c.path.contains(CACHE_FOLDER)) c.path else s"$CACHE_FOLDER/${c.path}"
              val localfilename = path.split("""/""").reverse.head
              s"$CACHE_FOLDER/${id}_$localfilename"
            }).getOrElse(""), debug = true, forceDownload = true)
            val istargetExists = new java.io.File(a).exists()
            if (istargetExists) (a, b) else {
              val tmpPath = jd.refLink.split("/").reverse.headOption.getOrElse("")
              val tmptarget = {
                val path = if (tmpPath.contains(CACHE_FOLDER)) tmpPath else s"$CACHE_FOLDER/${tmpPath}"
                val localfilename = path.split("""/""").reverse.head
                s"$CACHE_FOLDER/${id}_$localfilename"
              }
              val (a: String, b: String) = SiteParserAndCrawler.downloadAndCacheUrlToFile(jd.refLink, tmptarget, debug = true, forceDownload = true)
              (a, b)
            }
          }
          //val link = fixPdfBaseUrl(routes.SiteAndPageController.getFileByTagAndId("ads", jd._key.getOrElse(""), jd.content.map(_.name).getOrElse("")).absoluteURL())
          println(s"PDF link = $filePath for Job id = $id with downloaded filePath = $filePath")
          try {
            val prefixUrl = finalUrl
              .replaceAll("""https://""", "")
              .replaceAll("""http://""", "")
              .split("/")
              .toList
              .headOption
              .getOrElse("")
            val name = jd.content.map(_.name).getOrElse("")
            val ext = name.split("""\.""").reverse.head
            val s3filename = "pdf/" + prefixUrl + "/" + shared.utils.Slug.slugify(name) + s".$ext"
            app.utils.Utils.uploadToS3(
              filePath,
              s3filename
            ) match {
              case Right(value) =>
                val newFilename = filePath.split("""/""").reverse.head
                val refLink = fixPdfBaseUrl(routes.SiteAndPageController.getFileByTagAndId("ads", jd._key.getOrElse(""), newFilename).absoluteURL())
                val updatedJob = jd.copy(publicUrl = Some(value), refLink = refLink)
                val jobStatus = repo.save(updatedJob)
                Await.result(jobStatus, Duration.Inf)
                println(s"JOB updated with new refLink = ${refLink} and publicUrl = ${value}")
                value
              case Left(e) =>
                println(e)
            }
          } catch {
            case e: Exception =>
              //e.printStackTrace()
              println(s"S3 upload for target file = $filePath has error")
          }
          Future.successful(if (!filePath.isEmpty) Right(jd) else Left("Document cannot be downloaded"))
        } else Future.successful(Left(s"Job not found with ID = $id"))
      }
    } yield {
      runResults
    }
    result.map(r => {
      if (r.isRight) {
        Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Job file has been processed", link = Some(r.right.get.refLink))))
      } else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Job file cannot be processed", exception = Some(r.left.get))))
    })
  }

  def processAds(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val fulljobDao = new FullJobDAO(db, es)
    val result: Future[(SitePage, Either[String, Seq[Job]])] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      mayjobs <- fulljobDao.findByID(KeyID(_key = Some(id)))
      jobs <- {
        if (mayjobs.isDefined) {
          val currentAd = mayjobs.get
          if (currentAd.originalLink.isDefined) {
            Future.successful(mayjobs)
          } else {
            val isParse = if (isOverrideExistingGooglePdfFetchedDocs) true else {
              !currentAd.googleSearchResult.isDefined
            }
            //val fulljd = if (isParse) getOriginalAdLink(currentAd) else currentAd
            val fulljd = currentAd
            fulljobDao.save(fulljd).map(j => Some(j))
            //Future.successful(None)
          }
        } else Future.successful(None)
      }
      matches <- new RuleDAO(db, es).findByQuery(Seq.empty, 0, 99999)
      emps <- new EmployerDAO(db, es).findByQuery(Seq.empty, 0, 99999)
      runResults <- {
        if (jobs.isDefined) {
          val jd = jobs.get
          println(s"Parsing Ad to Jobs with ID = ${jd._key}")
          val parsedJobsF: Future[Either[String, Seq[Job]]] = parseFullJob(jd, emps, matches)
          val parsedJobs = Await.result(parsedJobsF, Duration.Inf)
          val xs: Either[String, Seq[Job]] = parsedJobs match {
            case Right(value) =>
              val ff = value.map(j => {
                val link = fixPdfBaseUrl(routes.SiteAndPageController.getFileByTagAndId("ads", jd._key.getOrElse(""), jd.content.map(_.name).getOrElse("")).absoluteURL())
                val savedjob = Await.result(apps.Helpers.saveJob(j.copy(fetchStatus = Some("active")), link), Duration.Inf)
                println(s"Saved JOB ID = ${savedjob._key} and j ID = ${j._key}")
                savedjob
              })
              println(s"Ad found with ID = $id and Job size = ${ff.size}")
              Right(ff)
            case Left(error) =>
              Left(error)
          }
          Future.successful(xs)
        } else Future.successful(Left(s"Ad not found with ID = $id"))
      }
    } yield {
      (page.getOrElse(SitePage.default), runResults)
    }
    result.map(r => {
      if (r._2.isRight) {
        //val link = r._2.right.get.head.refLink
        val jid = r._2.right.get.map(j => j._key.getOrElse("")).filter(s => !s.isEmpty).headOption.getOrElse("")
        val link = s"/jobs/$jid"
        println(s"Ref link for parsed Job = $link")
        Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Job Ad has been processed", link = Some(link))))
      } else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Job Ad cannot be processed", exception = Some(s"ID cannot be found or ${r._2.left.get}"))))
    })
  }
  def helperProcessSingleAd(fulljob: FullJob, isReParsing: Boolean = false, crawler: Option[SiteCrawler], adLink: String) = {
    val sampleCount = 0
    val adRepo = new FullJobDAO(db, es)
    val result = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- {
        val params = Seq(("source.url", fulljob.source.url.getOrElse("")))
        adRepo.findByQuery(params, 0, if (sampleCount > 0) sampleCount else 99999)
      }
      matches <- new RuleDAO(db, es).findByQuery(Seq.empty, 0, 99999)
      emps <- new EmployerDAO(db, es).findByQuery(Seq.empty, 0, 99999)
    } yield {
      (page.getOrElse(SitePage.default), jobs.toList, matches, emps)
    }
      val records = Await.result(result, Duration.Inf)
      val jobs = records._2
      val jdsSize = jobs.size
      val emps = records._4
      val matches = records._3
      println(s"Starting to process FullJob records = ${jdsSize}")
      val output: List[scala.concurrent.Future[List[shared.Job]]] = jobs.map(jd => {
        val id = jd._key.getOrElse("")
        app.utils.Utils.writeLog("/tmp/ads_status.txt", s"$id started", true)
          val parsedJobs: Future[Either[String, Seq[Job]]] = parseFullJob(jd, emps, matches, crawler)
          val fresult = for {
            parsed <- parsedJobs
            vxs <- {
              Future {
                val r:Seq[Job] = parsed match {
                  case Right(value) =>
                    value.map(j => {
                      println(s"Job created for url = ${jd.source.url}")
                      Await.result(apps.Helpers.saveJob(j.copy(fetchStatus = if (isReParsing) Some("active") else j.fetchStatus), adLink), Duration.Inf)
                      //Await.result(adRepo.save(jd.copy(status = Some("Done"))), Duration.Inf)
                      //Future.successful(Right(Seq(r)))
                    }).toList
                  case Left(error) =>
                    val msg = s"$id [$error] ended"
                    app.utils.Utils.writeLog("/tmp/ads_status.txt", msg, true)
                    println(s"FullJob or Job ended with error for url = ${jd.source.url}")
                    //Future.successful(Left(error))
                    //Await.result(), Duration.Inf)
                    Seq.empty
                }
                r
              }
            }
            done <- {
              Await.result(adRepo.save(jd.copy(status = Some("Done"))), Duration.Inf)
              Future.successful(vxs.toList)
            }
          } yield done
          fresult
      })
      Future.sequence(output).map(xs => xs.flatten)
  }


  def processAllAds = Action.async { implicit request: Request[AnyContent] =>
    val sampleCount = app.utils.Utils.decodeQueryString(request.queryString.get("sample").flatMap(_.headOption).getOrElse("999999")).toInt
    val adRepo = new FullJobDAO(db, es)
    val result = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- {
        adRepo.findByQuery(Seq(("status", "Pending")), 0, if (sampleCount > 0) sampleCount else 99999)
      }
      matches <- new RuleDAO(db, es).findByQuery(Seq.empty, 0, 99999)
      emps <- new EmployerDAO(db, es).findByQuery(Seq.empty, 0, 99999)
    } yield {
      (page.getOrElse(SitePage.default), jobs.toList, matches, emps)
    }
      val records = Await.result(result, Duration.Inf)
      val jobs = records._2
      val jdsSize = jobs.size
      val emps = records._4
      val matches = records._3
    Future {
      jobs.map(jd => {
        val id = jd._key.getOrElse("")
        app.utils.Utils.writeLog("/tmp/ads_status.txt", s"$id started", true)
        Future {
          val parsedJobs: Future[Either[String, Seq[Job]]] = parseFullJob(jd, emps, matches)
          val fresult = for {
            parsed <- parsedJobs
            v <- {
              Future {
                val r:Either[String, Seq[Job]] = parsed match {
                  case Right(value) =>
                    Right(value.map(j => {
                      val link = fixPdfBaseUrl(routes.SiteAndPageController.getFileByTagAndId("ads", jd._key.getOrElse(""), jd.content.map(_.name).getOrElse("")).absoluteURL())
                      Await.result(apps.Helpers.saveJob(j.copy(fetchStatus = Some("active")), link), Duration.Inf)
                      //Await.result(adRepo.save(jd.copy(status = Some("Done"))), Duration.Inf)
                      //Future.successful(Right(Seq(r)))
                    }).toList)
                  case Left(error) =>
                    app.utils.Utils.writeLog("/tmp/ads_status.txt", s"$id [$error] ended", true)
                    //Future.successful(Left(error))
                    //Await.result(), Duration.Inf)
                    Left(error)
                }
                r
              }
            }
            done <- adRepo.save(jd.copy(status = Some("Done")))
          } yield done.status

        }(myExecutionContext)
      })
    }(myExecutionContext)
    println(s"No of Ads found for processing = ${jobs.size} with sample = $sampleCount")
    Future.successful(Ok(Json.toJson(shared.SuccessResponse(code = 200, message = s"$jdsSize", exception = Some("Ads processing initiated")))))
  }


  def deleteAds(id: String) = Action.async { implicit request: Request[AnyContent] =>
    val result: Future[(SitePage, Boolean)] = for {
      page <- sitePage.findByKeyValue("siteName", "ads")
      jobs <- new FullJobDAO(db, es).deleteByID(KeyID(_key = Some(id)))
    } yield {
      println(s"deleteAds: $jobs")
      (page.getOrElse(SitePage.default), jobs)
    }
    result.map(r => {
      if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record has been deleted"))) else
        Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record cannot be deleted", exception = Some("ID cannot be found."))))
    })
  }

  def jobs = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val searchQ = {
        val tmp = searchQuery.getOrElse("")
        if (tmp.equalsIgnoreCase("""""""")) "" else tmp
      }
      val offsetLimit = offset
      val currentOffset = if (offsetLimit < APP_OFFSET) {APP_OFFSET / 2} else offsetLimit
      val dropNo = (pageNo - 1) * currentOffset
      println(s"BO jobs searchQ input size = ${searchQ.size}, offsetLimit = $offsetLimit, currentOffset = $currentOffset and dropNo = $dropNo")
      val keyEmp = app.utils.Utils.decodeQueryString(request.queryString.get("key").flatMap(_.headOption).getOrElse(""))
      val statusQ = app.utils.Utils.decodeQueryString(request.queryString.get("status").flatMap(_.headOption).getOrElse(""))
      val stateQ = app.utils.Utils.decodeQueryString(request.queryString.get("state").flatMap(_.headOption).getOrElse(""))
      val jobStatus = {
        val js = app.utils.Utils.decodeQueryString(request.queryString.get("jobStatus").flatMap(_.headOption).getOrElse(""))
        if (js.isEmpty) {
          if (searchQ.isEmpty)
            "active"
          else {
            if (js.equalsIgnoreCase("false")) "active" else js
          }
        } else {
          if (js.equalsIgnoreCase("false")) "active" else js
        }
      }
      val dateQuery = app.utils.Utils.decodeQueryString(request.queryString.get("date").flatMap(_.headOption).getOrElse(""))
      val jobQuery: Seq[(String, String)] = {
        val q = if (dateQuery.isEmpty) Seq.empty else Seq(("extras.dates.created", dateQuery))
        //val q = if (dateQuery.isEmpty) Seq(("fetchStatus", "active")) else Seq(("fetchStatus", "active"), ("extras.dates.created", dateQuery))
        val finalQ: Seq[(String, String)] = {
          val tmp = if (statusQ.isEmpty) q else {
            if (dateQuery.isEmpty) Seq(("status", statusQ), ("fetchStatus", statusQ)) else Seq(("status", statusQ), ("fetchStatus", statusQ), ("extras.dates.created", dateQuery))
          }
          val jobStatusList: Seq[(String, String)] = jobStatus match {
            case "Live" =>
              Seq(("status", "Live"))
              //Seq(("status", "Live"), ("~extras.employer.status", "Live"))
            case "Pending" =>
              Seq(("fetchStatus", "Pending"), ("~status", "Live"), ("~status", "Ignored"), ("~status", "Validated"))
            case "active" =>
              Seq(("fetchStatus", "active"), ("~status", "Live"), ("~status", "Ignored"), ("~status", "Validated"), ("~extras.employer.status", "Live"))
            case _ =>
              Seq.empty[(String, String)]
          }
          if (stateQ.isEmpty) {
            tmp ++ jobStatusList
          } else {
            Seq(("state", stateQ)) ++ tmp ++ jobStatusList
          }
        }
        finalQ
      }
        val empDAO = new EmployerDAO(db, es)
        val result: Future[(SitePage, Long, Seq[Job], Seq[DropdownValue], Seq[NameValue])] = for {
        page <- sitePage.findByKeyValue("siteName", "backoffice jobs")
        skills <- new EducationDAO(db, es).findByQuery(Seq.empty, 0, 9999)
        totals <- {
          //repo.findByQuery(Seq(("fetchStatus", "active")), 0, 99999).map(_.size.toLong)
          repo.findByQueryForCount(jobQuery, 0, 99999).map(_.toLong)
        }
        statesList <- repo.searchStates(es, "", searchQuery = "", offset = 0, count = 9999)
        jobs <- if (searchQ.isEmpty) {
          println(s"BO jobs date query = $dateQuery and key = $keyEmp without searchQ = $searchQ")
          if (keyEmp.isEmpty) {
            val finalQ = jobQuery
            val condType = if (!statusQ.isEmpty) "OR" else "AND"
            println(s"Jobs final Q = $finalQ for condition = $condType, dropNo = $dropNo and offset = $offset")
            repo.findByQuery(finalQ, dropNo, currentOffset, conditionType = condType).map(xs => {
              //println(s"SIZE xs = ${xs.size}")
              (totals, xs)
            })
          } else {
            repo.findByID(KeyID(_key = Some(keyEmp), id = None)).map(xs => {
              xs match {
                case Some(emp) => (1L, Seq(emp))
                case _ => (0L, Seq.empty)
              }
            })
          }
        } else {
          println(s"Search: BO jobs date query = $dateQuery and key = $keyEmp with searchQ = $searchQ")
          val dtQ = if (dateQuery.isEmpty) "" else s"""extras.dates.created:"$dateQuery""""
          val tmpQ = searchQ match {
            case "fetchStatus_Pending" => 
                  """fetchStatus:"Pending" ~(status:"Live") ~(status:"Ignored") ~(status:"Validated")"""
            case "fetchStatus_active" => 
            """fetchStatus:"active" ~(status:"Live") ~(status:"Ignored") ~(status:"Validated")"""
            case "status_Live" => 
             """status:"Live""""
              //"""status:"Live" ~(extras.employer.status:"Live")"""
            case _ => searchQ
          }
          val prefinalQ = {
             val jobStatusQ = jobStatus match {
                case "Live" =>
                  //"""status:"Live""""
                  """status:"Live" ~(extras.employer.status:"Live")"""
                case "Pending" =>
                  """fetchStatus:"Pending" ~(status:"Live") ~(status:"Ignored") ~(status:"Validated")"""
                case "active" =>
                  """fetchStatus:"active" ~(status:"Live") ~(status:"Ignored") ~(status:"Validated")"""
                case _ =>
                ""
              }
            val tmp = if (dtQ.isEmpty) tmpQ else s"$tmpQ AND $dtQ"
            val out = if (stateQ.isEmpty) tmp else s"""state:"$stateQ" $tmp""" 
            if (jobStatusQ.isEmpty) out else s"""$out $jobStatusQ"""
          }
          val finalQ = makeProperSearchQuery(prefinalQ, statusQ)
          println(s"Search Q (pre Q = $prefinalQ) before getting results from ES = $finalQ")
          try {
            repo.search(es, "", searchQuery = finalQ, dropNo, currentOffset)
          } catch {
            case e: Throwable =>
              e.printStackTrace()
              Future.successful((0L, Seq.empty))
          }
        }
      } yield {
        val states = statesList._2.map(s => Json.parse(s).validate[NameValue].get)
        //val states: Seq[NameValue] = Seq.empty
        (page.getOrElse(SitePage.default), jobs._1, jobs._2, skills.sortBy(e => s"""${e.category.getOrElse("")} - ${e.name}""").map(s => DropdownValue(name = s"""${s.category.getOrElse("")} - ${s.name}""", value = s.name, text = s.name)), states)
      }
      result.map(r => {
        val total = r._2
        val records = r._3
        println(s"Jobs final Records count: ${records.size}")
        val actualCount = r._3.size
        val userPageNo = ((pageNo - 1)* currentOffset) + actualCount
        val countMessage = if (userPageNo <= total) s"${userPageNo}/${total}" else s"$total/$total"
        implicit val pagination = Pagination(page = pageNo, offset = currentOffset, total = total.toInt)
        println(s"BO Jobs: Pagination = $pagination with drop = $dropNo, actual count = $actualCount and user page no = $userPageNo / $total with countMessage = $countMessage")
        val paramsMap: Map[String, String] = Map("state" -> stateQ, "search" -> searchQ,"jobStatus" -> jobStatus, "key" -> keyEmp, "status" -> statusQ, "date" -> dateQuery, "userMessage" -> countMessage)
        Ok(backofficeJobsView(r._1, assetsFinder, records, r._4, statusQ, paramsMap, r._5))
      }).recover({
        case e: Throwable =>
          e.printStackTrace()
          val error = app.utils.Utils.stackTrace(e)
        val r = sitePage.findByKeyValue("siteName", "jobs").map(page => {
          println(s"Page found: $page")
          Ok(errorView(page.getOrElse(SitePage.default), assetsFinder, error))
        })
          Await.result(r, Duration.Inf)
      })
    })
  }

  def saveJobs = Action.async(parse.json) { implicit request =>
      val payload = request.body
      println(s"Backoffice: Request for updating Job with payload = $payload")
      try {
        payload.validate[Job] match {
          case JsSuccess(value, path) =>
            val empDao = new EmployerDAO(db, es)
            val educationDAO = new EducationDAO(db, es)
            val result: Future[Job] = for {
              objs <- repo.findByID(KeyID(_key = value._key))
              educations <- {
                val qs = Seq.empty
                educationDAO.findByQuery(qs, 0, 99999, conditionType = "OR").map(xs => {
                  xs.filter(e => value.educations.contains(e.name))
                })
              }
              maybeEmployer <- empDao.findByID(KeyID(_key = Some(value.company)))
              //maybeEmployer <- empDao.findByQuery(Seq(("_key", value.company), ("company", value.company)), 0, 1).map(emps => emps.headOption)
              saveresult <- {
                //println(s"Employer found = ${maybeEmployer}")
                val stateFromEmp = maybeEmployer.map(_.extra.state).getOrElse("")
                val deptFound = maybeEmployer.map(_.extra.department.getOrElse("")).getOrElse(value.extras.map(_.department).getOrElse(""))
                val websiteFound = maybeEmployer.map(_.website).getOrElse(value.extras.map(_.website).getOrElse(""))
                val location = (payload \ "location").as[String]
                val skills: Seq[EducationTag] = if (!value.educations.isEmpty) educations.map(xs => xs.toEducationTag()) else Seq.empty
                //println(s"Location = $location")
                if (objs.isDefined) {
                  val j = objs.get
                  val originalPdfTmp = (payload \ "originalPdf").as[String]
                  val originalPdf =  if (originalPdfTmp.isEmpty) j.extras.map(_.originalPdf).getOrElse("") else originalPdfTmp
                  val posts = (payload \ "noPosts").as[String] match {
                    case "" => 0
                    case _ @ no => no.toInt
                  }
                  val loc = Some(Location(name = location,landmark = None, geocode = None))
                  val dt = DatesInfo.getCurrentDatesInUTC()
                  val stats = j.extras.map(_.stats).getOrElse(Some(JobStats.empty)).get.copy(noPosts = if (posts <= 0) None else Some(posts))
                  val extra = JobExtra(location = loc, originalPdf = originalPdf, originalLabel = None, website = maybeEmployer.map(_.website).getOrElse(""), department = maybeEmployer.map(_.extra.department.getOrElse("")).getOrElse(""), educations = List.empty, employer = maybeEmployer, possibleDates = List.empty, dates = dt)
                  val extras = j.extras.getOrElse(extra).copy(website = websiteFound, department = deptFound, educations = skills, location = loc, originalPdf = originalPdf, stats = Some(stats))
                  val updatedExtras = if (maybeEmployer.isDefined) extras.copy(employer = maybeEmployer, originalPdf = originalPdf) else extras
                  //val extras: Option[JobExtra] = objs.get.extras.map(e => e.copy(examDate = examDate.flatten))
                  val company = maybeEmployer.map(_.name).getOrElse(j.company)
                  println(s"Found employer = $maybeEmployer to be fixed with company = $company for job id = ${value._key}")
                  val latestJob = value.copy(state = if (!value.state.getOrElse("").isEmpty) value.state else Some(stateFromEmp), company = company, content = j.content, extras = Some(updatedExtras), fetchStatus = j.fetchStatus)
                  println(s"Old Job = ${latestJob.title} updated with publicurl = ${latestJob.publicUrl} and refLink = ${latestJob.refLink}")
                  //val updateES = latestJob.status.getOrElse("").equalsIgnoreCase("Live")
                  repo.save(latestJob)
                } else {
                  val originalPdf = (payload \ "originalPdf").as[String]
                  val posts = (payload \ "noPosts").as[String] match {
                    case "" => 0
                    case _ @ no => no.toInt
                  }
                  val stats = JobStats.empty.copy(noPosts = if (posts <= 0) None else Some(posts))
                  val loc = Some(Location(name = location,landmark = None, geocode = None))
                  val dt = DatesInfo.getCurrentDatesInUTC()
                  val extra = JobExtra(location = loc, originalPdf = originalPdf, originalLabel = None, website = maybeEmployer.map(_.website).getOrElse(""), department = maybeEmployer.map(_.extra.department.getOrElse("")).getOrElse(""), educations = List.empty, employer = maybeEmployer, possibleDates = List.empty, dates = dt)
                  val extras = value.extras.getOrElse(extra).copy(website = websiteFound, department = deptFound, educations = skills, location = loc, originalPdf = originalPdf, stats = Some(stats))
                  val updatedExtras = if (maybeEmployer.isDefined) extras.copy(employer = maybeEmployer, originalPdf = originalPdf) else extras
                  val latestJob = value.copy(state = if (!value.state.getOrElse("").isEmpty) value.state else Some(stateFromEmp), company = maybeEmployer.map(_.name).getOrElse(value.company), content = value.content, extras = Some(updatedExtras), fetchStatus = value.fetchStatus)
                  //val updateES = value.status.getOrElse("").equalsIgnoreCase("Live")
                  println(s"Job = ${latestJob.title} updated with publicurl = ${latestJob.publicUrl} and refLink = ${latestJob.refLink}")
                  repo.save(latestJob)
                }
              }
            } yield {
              //println(s"Got job: $jobs")
              saveresult
            }
            result.map(r => {
              Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200)))
            })
          case JsError(errors) =>
            val error = errors.map(e => s"${e._1.path.toString()}: ${e._2.toList.mkString(",")}").mkString(", ")
            println(s"Job save invalid JSON error: ${errors} for input json = ${payload}")
            Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, s"Save error: $error", false, Some(errors.toString())))))
        }
      } catch {
        case e: Exception =>
          val error = app.utils.Utils.stackTrace(e)
          println(s"Job save error: ${error} for input json = ${payload}")
          Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, s"Save error: $error", false, Some(error)))))
      }
  }

  def deleteJobs(id: String) = Action.async { implicit request: Request[AnyContent] =>
      val ids = if (id.contains(",")) id.split(",").toList else List(id)
    println(s"BO: Delete jobs = $ids")
      val result: Future[(SitePage, Boolean)] = for {
        page <- sitePage.findByKeyValue("siteName", "ads")
        jobs <- {
          val values = ids.map(id => {
            //val r = repo.deleteByID(KeyID(_key = Some(id)))
            val r = repo.findByID(KeyID(_key = Some(id)))
            val job = Await.result(r, Duration.Inf)
            if (job.isDefined) {
              val dates = job.get.extras.map(_.dates).getOrElse(DatesInfo.getCurrentDatesInUTC()).copy(deleted = Some(ZonedDateTime.now()))
              val updatedJob = job.get.copy(status = Some("Ignored"), extras = job.get.extras.map(_.copy(dates = dates)))
              Await.result(repo.save(updatedJob), Duration.Inf).status.isDefined
            } else false
          })
          Future.successful(values.distinct.contains(true))
        }
      } yield {
        println(s"Deleted job: $jobs")
        (page.getOrElse(SitePage.default), jobs)
      }
      result.map(r => {
        if (r._2) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Record(s) has been deleted"))) else
          Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Record(s) cannot be deleted", exception = Some("ID cannot be found."))))
      })
  }


  def search(index: String) = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val attributeName: String = app.utils.Utils.decodeQueryString(request.queryString.get("attributeName").flatMap(_.headOption).getOrElse(""))
      val query: String = app.utils.Utils.decodeQueryString(request.queryString.get("query").flatMap(_.headOption).getOrElse(""))
      val limit: Int = app.utils.Utils.decodeQueryString(request.queryString.get("limit").flatMap(_.headOption).getOrElse("999999999")).toInt
      val dropNo = (pageNo - 1) * offset
      val empDao = new EmployerDAO(db, es)
      val fullDao = new FullJobDAO(db, es)
      val result: Future[(SitePage, Long, Seq[FullJob])] = for {
        page <- sitePage.findByKeyValue("siteName", "jobs")
        jobs <- fullDao.search(es, attributeName, query, dropNo, limit)
      } yield {
        (page.getOrElse(SitePage.default), jobs._1, jobs._2)
      }
      result.map(r => {
        println(s"No of records matched: ${r._2}")
        val total = r._2
        val records = r._3
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total.toInt)
        println(s"Pagination = $pagination with drop = $dropNo")
        Ok(backofficeAdsView(r._1, assetsFinder, records))
      })
    })
  }

  def parseEducationSkills(filename: String) = {
    val datas: scala.collection.mutable.ListBuffer[Education] = scala.collection.mutable.ListBuffer.empty
    val lines = scala.io.Source.fromFile(filename).getLines()
    var category = ""
    lines.foreach(line => {
      if (!line.trim.isEmpty) {
        if (category.isEmpty) {
          category = line.trim
        } else {
          val tokens = line.trim.split("""/""").toList.map(s => s.trim).filter(!_.isEmpty)
          val edu = Education(id = None, _key = None, category = Some(category), code = line.trim, name = line.trim, tags = List(category) ++ tokens)
          datas.append(edu)
        }
      } else {
        category = ""
      }
    })
    datas.toList
  }

  def loadEducationAndSkills = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
      println(s"Load and parse name: $name")
      import shared.utils.Slug._
      val actorname = name.slug
      val cacheFolder = CACHE_FOLDER
      name.isEmpty match {
        case true =>
          val eduDao = new EducationDAO(db, es)
          /*
        val xs = api.JobParser.EDUCATION_MAP.map(v => {
          Education(id = None, _key = None, code = v._1, name = v._1, tags = v._2)
        })
         */
          val xs = parseEducationSkills(CACHE_FOLDER + "/education_skills.txt")
          println(xs)
          Future.sequence(xs.map(edu => {
            eduDao.save(edu)
          })).map(result => {
            Ok(s"No of records saved: ${result.size}")
          })
        case false =>
          import akka.pattern.ask
          implicit val timeout: akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
          val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
          val resultF: Future[List[Employer]] = {
            ref ? LoadExcel(name)
          }.mapTo[List[Employer]]
          if (ref != null) {
            ref ! Stop
          }
          val empDao = new EmployerDAO(db, es)
          val r = for {
            xs <- resultF
            savedresults <- Future.sequence(xs.map(x => empDao.save(x)))
          } yield {
            (xs, savedresults)
          }
          r.map(xs => {
            Ok(s"No of records loaded: ${xs._1.size} and saved: ${xs._2.size}")
          })
      }
    })
  }

  def makeNonEmptyRecordsAndSort(datas: List[String]): List[String] = {
    datas.toSet.toList.filter(x => !x.isEmpty && !x.replaceAll(" ", "").isEmpty).sortWith((a, b) => a < b)
  }

  def loadMatchingRules = Action.async { implicit request: Request[AnyContent] =>
    val dao = new RuleDAO(db, es)


    val r = for {
      maybeCat <- dao.findByKeyValue("name", "Specialisation")
      categories <- {
        maybeCat match {
          case Some(obj) =>
            dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(JobParser.SPECIALIZATION ++ obj.matches)))
          case _ =>
            val xs = MatchingRule(id = None, _key = None, rule = "specialisation", name = "Specialisation", matches = JobParser.SPECIALIZATION, matchType = "Education", category = "Specialisation")
            dao.save(xs)
        }
      }
      maybeLevel <- dao.findByKeyValue("name", "Level")
      levels <- {
        maybeLevel match {
          case Some(obj) =>
            dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(JobParser.LEVELS ++ obj.matches)))
          case _ =>
            val xs = MatchingRule(id = None, _key = None, rule = "level", name = "Level", matches = JobParser.LEVELS, matchType = "Education", category = "Level")
            dao.save(xs)
        }
      }
      maybe8 <- dao.findByKeyValue("name", "8th")
      matches8 <- {
        maybe8 match {
          case Some(obj) =>
            dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(JobParser.EDUCATION_8_CLASS ++ obj.matches)))
          case _ =>
            val xs = MatchingRule(id = None, _key = None, rule = "8th", name = "8th", matches = JobParser.EDUCATION_8_CLASS, matchType = "Education", category = "Education")
            dao.save(xs)
        }
      }
      maybe10 <- dao.findByKeyValue("name", "10th")
      matches10 <- {
        maybe10 match {
          case Some(obj) =>
            dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(JobParser.EDUCATION_10_CLASS_MATCHES ++ obj.matches)))
          case _ =>
            val xs = MatchingRule(id = None, _key = None, rule = "10th", name = "10th", matches = JobParser.EDUCATION_10_CLASS_MATCHES, matchType = "Education", category = "Education")
            dao.save(xs)
        }
      }
      maybe12 <- dao.findByKeyValue("name", "12th")
      matches12 <- {
        maybe12 match {
          case Some(obj) =>
            dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(JobParser.EDUCATION_12_CLASS_MATCHES ++ obj.matches)))
          case _ =>
            val xs = MatchingRule(id = None, _key = None, rule = "12th", name = "12th", matches = JobParser.EDUCATION_12_CLASS_MATCHES, matchType = "Education", category = "Education")
            dao.save(xs)
        }
      }
      maybeEducations <- {
        val xs: List[Future[MatchingRule]] = api.JobParser.EDUCATION_MAP.map(v => {
          val maybe = Await.result(dao.findByKeyValue("code", v._1), Duration.Inf)
          maybe match {
            case Some(obj) =>
              dao.save(obj.copy(matches = makeNonEmptyRecordsAndSort(List(v._1) ++ v._2 ++ obj.matches)))
            //Education(id = None, _key = None, code = v._1, name = v._2, tags = List(v._1, v._2, v._1.toLowerCase, v._2.toLowerCase))
            case _ =>
              val xs = MatchingRule(id = None, _key = None, rule = v._1, name = v._1, matches = makeNonEmptyRecordsAndSort(List(v._1) ++ v._2), matchType = "Education", category = "Education")
              dao.save(xs)
          }
        }).toList
        Future.sequence(xs)
      }
    } yield {
      (matches8, matches10, matches12)
    }
    r.map(result => {
      Ok(s"${result}")
    })
  }

  def saveCrawlingSiteFromEmployer(emp: Employer, urlList: List[String]) = {
    val crawlerDao = new SiteCrawlerDAO(db, es)
    urlList.map(link => {
      val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
      val url = link.toLowerCase.trim
      val sc = SiteCrawler(name = name, url = url, domain = Some(SiteUrl.getJustDomain(url)), ignore = Some("no"), priority = 1, status = "", originalUrl = Some(link), siteType = Some("sites"), result = None)
      val sites: Seq[SiteCrawler] =  Await.result(crawlerDao.findByQuery(Seq(("url", url)), 0, 999), Duration.Inf)
      val datas = if (sites.isEmpty) Seq(sc) else sites
      datas.map(c => {
        val r = crawlerDao.save(c.copy(url = url, originalUrl = Some(link)))
        Await.result(r, Duration.Inf)
      })
    })
  }

  //Employers upload via excel
  def loadAndParseExcel = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
      println(s"Load and parse name: $name")
      import shared.utils.Slug._
      val actorname = name.slug
      val cacheFolder = CACHE_FOLDER
      name.isEmpty match {
        case true =>
          Future.successful(Ok("No data provided"))
        case false =>
          import akka.pattern.ask
          implicit val timeout: akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
          val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
          val resultF: Future[List[Employer]] = {
            ref ? LoadExcel(name)
          }.mapTo[List[Employer]]
          if (ref != null) {
            ref ! Stop
          }
          val empDao = new EmployerDAO(db, es)
          val r = for {
            xs <- resultF
            savedresults <- Future.sequence(xs.map(x => {
              val empsList: Future[Seq[Employer]] = empDao.findByQuery(Seq(("website", x.website), ("domain", x.domain.getOrElse("")), ("name", x.name)), 0, 999, conditionType = "OR").map(emps => {
                emps.map(e => {
                  val emp = empDao.save(x.copy(_key = e._key, id = e.id))
                  println(s"Updated existing employer = ${x.name} and site = ${x.website}")
                  val r = Await.result(emp, Duration.Inf)
                  saveCrawlingSiteFromEmployer(r, r.extra.links.getOrElse(List.empty))
                  r
                })
              })
              val emps = Await.result(empsList, Duration.Inf)
              if (emps.isEmpty) empDao.save(x) else Future.successful(emps.head)
            }))
          } yield {
            (xs, savedresults)
          }
          r.map(xs => {
            Ok(s"No of records loaded: ${xs._1.size} and saved: ${xs._2.size}")
          })
      }
    })
  }

  val APP_BASE_HTTP_URL = {
    val url = System.getenv("APP_BASE_HTTP_URL")
    if (url == null) "" else url
  }

  val DOWNLOAD_AND_EXTRACT_CONTENT = true

  def fetchSingleJob(concurrencyNo: Int, offsetNo: Int, pageNo: Int, priority: Int, fetchAllParams: Boolean, name: String, url: String, linksToFetch: String, isFetchSingleOnly:Boolean) = {
    val cacheFolder = CACHE_FOLDER
    var jobFetchStatusFound: String = ""
    //implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    import zio._
    import zio.duration.{Duration => ZioDuration}
    val crawlerDao = new SiteCrawlerDAO(db, es)
    val linksTobefetched: List[SiteCrawler] = {
      val urlList: List[String] = linksToFetch.split(",").toList
      println(s"URL to be fetched and parsed = $urlList")
        val siteType = fetchAllParams match {
          case true => "competitors"
          case _ => "sites"
        }
      val crawlingSites = urlList.map(link => {
        val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
        SiteCrawler(name = name, url = link, domain = Some(SiteUrl.getJustDomain(link)), ignore = Some("no"), priority = priority, status = "", result = None, originalUrl = Some(link), siteType = Some(siteType))
      })
      crawlingSites.map(site => {
        val oldSites = Await.result(crawlerDao.findByQuery(Seq(("url", site.originalUrl.getOrElse(site.url))), 0, 999999), Duration.Inf)
        if (oldSites.isEmpty) Await.result(crawlerDao.save(site), Duration.Inf) else Await.result(crawlerDao.save(oldSites.head.copy(priority = priority, siteType = Some(siteType))), Duration.Inf)
      })
    }
    val sitesForFetching: List[SiteCrawler] = linksTobefetched.filter(site => {
      val job = Await.result(repo.findByKeyValue("extras.originalPdf", site.url), Duration.Inf)
      if (job.isDefined) {
        val jjj = job.get
        println(s"Found old Job with id = ${jjj._key} and title = ${jjj.title}")
        val nonLive = !jjj.status.getOrElse("").equalsIgnoreCase("Live")
        if (!nonLive) {
          if (isFetchSingleOnly) {
            val jid = jjj._key.getOrElse("")
            jobFetchStatusFound = s"Fetch Status: Already found with status LIVE with JID = ${jid}"
          }
        }
        nonLive
      } else true
    })
    val isReParsing = !sitesForFetching.isEmpty
    println(s"SITES to be processed with user input = $sitesForFetching and isReParsing = $isReParsing")
    //val datas = crawlerDao.findByQuery(Seq.empty, 0, offsetNo)
    val finalResultF = for {
      urlRulesNegatives <- ruledao.findByQuery(Seq(("rule", "negative_keywords")), 0, 99999, conditionType = "OR")
      titleRulesNegatives <- ruledao.findByQuery(Seq(("rule", "negative_keywords")), 0, 99999, conditionType = "OR")
      jobUrlRulesPositives <- ruledao.findByQuery(Seq(("rule", "positive_keywords")), 0, 99999, conditionType = "OR")
      jobTitleRulesPositives <- ruledao.findByQuery(Seq(("rule", "positive_keywords")), 0, 99999, conditionType = "OR")
      datas <- {
        val q = if (sitesForFetching.isEmpty) {
          if (!isFetchSingleOnly)
            Seq(("status", ""))
          else
            Seq(("status", "Already Done"))
        } else {
          sitesForFetching.map(s => ("url", s.url))
        }
        println(s"Q for fetching data for sites = $q")
        crawlerDao.findByQuery(q, 0, 999999, conditionType = "OR")
      }
      records <- {
        val records = datas.filter(r => {
          if (sitesForFetching.isEmpty) {
            r.status.equalsIgnoreCase("") || r.status.equalsIgnoreCase("pending")
          } else true
        }).drop(pageNo * offsetNo).take(offsetNo)
        println(s"Starting with total no of records = ${records.size} for offset no = $offsetNo")
        val negativeUrlKeywords: List[String] = urlRulesNegatives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim).sortBy(s => s)
        val negativeTitleKeywords: List[String] = titleRulesNegatives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim).sortBy(s => s)
        val positiveUrlKeywords: List[String] = jobUrlRulesPositives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim).sortBy(s => s)
        val positiveTitleKeywords: List[String] = jobTitleRulesPositives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim).sortBy(s => s)
        println(s"Got no of positive keywords = ${positiveTitleKeywords.size} and negative keywords = ${negativeTitleKeywords.size}")
        Future.successful(
          records.map(cx => {
            zio.Task.effect {
              val sites = List(cx)
              try {
                val xs: List[SiteCrawler] = sites.map(site => {
                  val url = site.originalUrl.getOrElse(site.url)
                  val (crawlerError: String, crawler: Option[SiteCrawlerResult]) = if (site.siteType.getOrElse("").equalsIgnoreCase("competitors"))
                    PartnerCrawler.parse(url, cacheFolder, DOWNLOAD_AND_EXTRACT_CONTENT, 0, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords, isReParsing = isReParsing)
                  else {
                    SiteParserAndCrawler.parse(url, cacheFolder, DOWNLOAD_AND_EXTRACT_CONTENT, 0, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords, isReParsing = isReParsing)
                  }
                  val obj = site.copy(result = crawler, status = "crawled")
                  val jobsF: Future[SiteCrawler] = for {
                    crawleddata <- SiteCrawlerDAO.saveAll(db, es, obj)
                    fulljobs <- {
                      println(s"Starting to get Ads for crawled site data = ${crawleddata.url}")
                      helperSiteCrawelerToFullJob(crawleddata, parsing = DOWNLOAD_AND_EXTRACT_CONTENT, isReParsing = isReParsing, fetchAllParams = fetchAllParams)
                    }
                    jobsAndSite <- {
                      println(s"No of FullJob to be processed for url = ${url} == ${fulljobs.size}")
                      val rrr = for {
                        _xs <- {
                          val jobsF: Seq[Future[List[Job]]] = fulljobs.map(fulljd => {
                            println(s"FullJob is to be processed for url = ${url} == ${fulljd._key}")
                            //routes.SiteAndPageController.getFileByTagAndId("ads", fulljd._key.getOrElse(""), fulljd.content.map(_.name).getOrElse("")).absoluteURL()
                            val adLink = s"""$APP_BASE_HTTP_URL/backoffice/files/ads/${fulljd._key.getOrElse("")}/${fulljd.content.map(_.name).getOrElse("")}"""
                            val link = fixPdfBaseUrl(adLink)
                            val _result: Future[List[Job]] = helperProcessSingleAd(fulljd, isReParsing = isReParsing, crawler = Some(obj), adLink = link)
                            _result
                          })
                          Future.sequence(jobsF)
                        }
                        updatedSite <- {
                          val priority = if (site.siteType.getOrElse("").equalsIgnoreCase("competitors")) {crawleddata.priority + fulljobs.size + _xs.size} else {fulljobs.size + _xs.size}
                          crawlerDao.save(crawleddata.copy(priority = priority, noAds = Some(fulljobs.size), noJobs = Some(_xs.size), status = "done", fullJobStatus = Some("done"), jobStatus = Some("done")))
                        }
                      } yield ((_xs, updatedSite))
                      rrr
                    }
                    jobSaveAndCrawlerUpdateF <- {
                      val jobxsSite = jobsAndSite
                      val jobxs = jobxsSite._1.toList.flatten.toList
                      println(s"No of Job found for url = $url == ${jobxs.size}")
                      if (jobxs.isEmpty) {
                        //Await.result(crawlerDao.save(crawleddata.copy(noAds = Some(fulljobs.size), noJobs = Some(0), status = "done", fullJobStatus = Some("done"), jobStatus = Some("done"))), scala.concurrent.duration.Duration.Inf)
                        Future.successful(jobxsSite._2)
                      } else {
                        val priority = if (site.siteType.getOrElse("").equalsIgnoreCase("competitors")) {crawleddata.priority + fulljobs.size + jobxs.size} else {fulljobs.size + jobxs.size}
                        crawlerDao.save(jobxsSite._2.copy(priority = priority, noJobs = Some(jobxs.size), status = "done", fullJobStatus = Some("done"), jobStatus = Some("done")))
                        //jobxsSite._2
                      }
                    }
                  } yield {
                    jobSaveAndCrawlerUpdateF
                  }
                  Await.result(jobsF, Duration(600, TimeUnit.SECONDS))
                })
                xs
              } catch {
                case e: Exception =>
                  val error = app.utils.Utils.stackTrace(e)
                  println(s"\n\nERROR for site craweler = $cx == $error\n\n")
                  List.empty
              }
            }
          })
        )
      }
    } yield records
    val allTasks: Seq[zio.ZIO[Any, Throwable, List[shared.SiteCrawler]]] = Await.result(finalResultF, Duration.Inf)
    println(s"Running concurrent tasks with concurrency no = $concurrencyNo and offsetNo = $offsetNo")
    val tasks = zio.Task.collectAllParN(concurrencyNo)(allTasks)
    //val runtime = new zio.DefaultRuntime() {}
    //(new DefaultRuntime() {}).unsafeRun(tasks)
    //runtime.unsafeRunAsync(tasks)(_)
    if (sitesForFetching.isEmpty) {
      Future {
        runtime.unsafeRun(tasks)
      }
      if (!jobFetchStatusFound.isEmpty)
        s"$jobFetchStatusFound^Data fetch requested accepted"
      else
      "Data fetch requested accepted"
    } else {
      val datas = runtime.unsafeRun(tasks)
      val output1 = datas.flatten.toList
      val r = output1.map(data => {
        val paths: Seq[String] = data.result.map(s => s.paths).getOrElse(Seq.empty)
        paths.map(p => {
          val url = s"""$APP_BASE_HTTP_URL/backoffice/jobs?search="$p""""
          url
        })
      }).flatten.mkString("\n")
      if (!jobFetchStatusFound.isEmpty)
        s"$jobFetchStatusFound^$output1\n\n${r.toString()}"
      else 
        s"$output1\n\n${r.toString()}"
    }
  }

  def testing = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val concurrencyNo: Int = app.utils.Utils.decodeQueryString(request.queryString.get("concurrency").flatMap(_.headOption).getOrElse("8")).toInt
      val offsetNo: Int = app.utils.Utils.decodeQueryString(request.queryString.get("offsetNo").flatMap(_.headOption).getOrElse("999999")).toInt
      val pageNo: Int = app.utils.Utils.decodeQueryString(request.queryString.get("pageNo").flatMap(_.headOption).getOrElse("0")).toInt
      val name: String = app.utils.Utils.decodeQueryString(request.queryString.get("filename").flatMap(_.headOption).getOrElse(""))
      val url: String = app.utils.Utils.decodeQueryString(request.queryString.get("url").flatMap(_.headOption).getOrElse(""))
      val linksToFetch: String = app.utils.Utils.decodeQueryString(request.queryString.get("urls").flatMap(_.headOption).getOrElse(""))
      val priority: Int = app.utils.Utils.decodeQueryString(request.queryString.get("priority").flatMap(_.headOption).getOrElse("1")).toInt
      val fetchAllParams: Boolean = app.utils.Utils.decodeQueryString(request.queryString.get("fetchAllParams").flatMap(_.headOption).getOrElse("false")).toBoolean
      val dateBefore: String = app.utils.Utils.decodeQueryString(request.queryString.get("date").flatMap(_.headOption).getOrElse(""))
      println(s"Load and parse name: $name")
      val ruledao = new RuleDAO(db, es)
      import shared.utils.Slug._
      val cacheFolder = CACHE_FOLDER
      url.isEmpty match {
        case true =>
          if (name.equalsIgnoreCase("search")) {
            val g = api.GoogleSearch.searchAndDownload(APP_GOOGLE_SEARCH_API_KEYS, cacheFolder, "")
            println(s"Ignoring pdf from sites: $APP_IGNORE_PDF_FROM_SITES")

            import akka.pattern.ask
            implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
            g.get.value.zipWithIndex.map(url => {
              val actorname = url._1.slug
              val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"google-file-download-actor-$actorname")
              val targetName = s"""${cacheFolder}pdf/originals/${g.get.names(url._2)}"""
              val r = ref ? DownloadFile(url._1, targetName)
              println(r)
            })
            Future.successful(Ok("No data provided"))

          }
          else if (name.equalsIgnoreCase("change-jobs-status")) {
            Future {
              val dt = if (!dateBefore.trim.isEmpty) {
                val currentDate = app.utils.Utils.getZonedDateTime(dateBefore)
                val objs = repo.findByQuery(Seq.empty, 0, 99999)
                val jobs1 = Await.result(objs, Duration.Inf).filter(job => {
                  val live = job.status.getOrElse("").equalsIgnoreCase("Live")
                  val pending = job.status.getOrElse("").equalsIgnoreCase("Pending")
                  val ignored = job.status.getOrElse("").equalsIgnoreCase("Ignored")
                  val validated = job.status.getOrElse("").equalsIgnoreCase("Validated")
                  //pending || ignored || validated
                  !live
                })
                println(s"No of non LIVE jobs = ${jobs1.size}")
                val jobs2 = jobs1.filter(job => {
                  val dates = job.extras.map(_.dates).getOrElse(DatesInfo.getCurrentDatesInUTC())
                  println(s"Job ${job._key} for date check = ${dates.created}")
                  dates.created.toInstant.toEpochMilli <= currentDate.toInstant.toEpochMilli
                })
                println(s"No of jobs before date ${dateBefore} = ${jobs2.size}")
                  jobs2.map(job => {
                  Await.result(repo.save(job.copy(fetchStatus = Some("Pending"))), Duration.Inf)
                  println(s"Job status updated for ${job._key}")
                })
              }
              Ok(s"Jobs status update started for ${dateBefore}")
            }
          }
          else if (name.equalsIgnoreCase("remove-jobs")) {
            Future {
              val objs = repo.findByQuery(Seq(("extras.dates.created", "2020-07-07")), 0, 99999)
              Await.result(objs, Duration.Inf).map(job => {
                if (!job.status.getOrElse("").equalsIgnoreCase("Live")) {
                  repo.deleteByID(KeyID(_key = job._key))
                  println(s"Deleted Job = ${job.title} and ID = ${job._key}")
                }
              })
            }
            Future.successful(Ok("Jobs migration for Skills started"))
          }
          else if (name.equalsIgnoreCase("fix-jobs-for-skills")) {
            Future {
              val educationDAO = new EducationDAO(db, es)
              val employers = Await.result(educationDAO.findByQuery(Seq.empty, 0, 99999), Duration.Inf)
              val table: scala.collection.mutable.Map[String, Education] = scala.collection.mutable.Map.empty
              employers.map(emp => {
                table.get(emp.name.toLowerCase) match {
                  case Some(found) =>
                    table.put(found.name.toLowerCase, found)
                  case _ =>
                    table.put(emp.name.toLowerCase, emp)
                }
              })
              val objs = repo.findByQuery(Seq(("fetchStatus", "active")), 0, 99999)
              val jobs = Await.result(objs, Duration.Inf).map(job => {
                  val eduxs = job.educations.filter(edu => {
                    table.get(edu.toLowerCase).isDefined
                  })
                  val xs = eduxs.map(e => table.get(e.toLowerCase).get).map(_.toEducationTag())
                val extras = job.extras.map(j => j.copy(educations = xs))
                val obj = job.copy(educations = eduxs, extras = extras)
                Await.result(repo.save(obj), Duration.Inf)
                println(s"Updated Job = ${obj.title} and ID = ${job._key}")
              })
            }
            Future.successful(Ok("Jobs migration for Skills started"))
          }
          else if (name.equalsIgnoreCase("rules-to-skills")) {
            Future {
              val ruleDAO = new RuleDAO(db, es)
              val educationDAO = new EducationDAO(db, es)
              val employers = Await.result(ruleDAO.findByQuery(Seq.empty, 0, 99999), Duration.Inf)
              val table: scala.collection.mutable.Map[String, MatchingRule] = scala.collection.mutable.Map.empty
              employers.map(emp => {
                table.get(emp.rule.toLowerCase) match {
                  case Some(found) =>
                    table.put(found.rule.toLowerCase, found)
                  case _ =>
                    table.put(emp.rule.toLowerCase, emp)
                }
              })
              employers.map(emp => {
                val objs = educationDAO.findByKeyValue("name", emp.rule)
                val found = Await.result(objs, Duration.Inf)
                val obj = if (found.isDefined) {
                  found.get.copy(tags = emp.matches.toSeq, category = Some(emp.category), level = emp.level, description = emp.description)
                } else {
                    emp.toEducation()
                }
                println(s"Saving Education = $obj")
                Await.result(educationDAO.save(obj), Duration.Inf)
              })
            }
            Future.successful(Ok("Skills migration started"))
          }
          else if (name.equalsIgnoreCase("remove-duplicate-employers")) {
            Future {
              val employerDAO = new EmployerDAO(db, es)
              val employers = Await.result(employerDAO.findByQuery(Seq.empty, 0, 99999), Duration.Inf)
              val table: scala.collection.mutable.Map[String, Employer] = scala.collection.mutable.Map.empty
              employers.map(emp => {
                val domain = Employer.getWebsite(emp.website)
                table.get(domain) match {
                  case Some(found) =>
                    val oldlinks = found.extra.links.getOrElse(List.empty).size
                    val links = emp.extra.links.getOrElse(List.empty).size
                    val e = if (links > oldlinks) emp else found
                    table.put(domain, e.copy(domain = Some(Employer.getWebsite(e.website))))
                  case _ =>
                    table.put(domain, emp.copy(domain = Some(Employer.getWebsite(emp.website))))
                }
              })
              employers.map(emp => {
                val obj = employerDAO.deleteByID(KeyID(_key = emp._key))
                Await.result(obj, Duration.Inf)
              })
              val count = table.values.toList.map(emp => {
                val obj = employerDAO.save(emp)
                val r = Await.result(obj, Duration.Inf)
                println(s"Saved employer = ${r.website}")
                r
              }).size
              println(s"No of employers saved = $count")
            }
            Future.successful(Ok("Employers duplicates removal started"))
          }
          else if (name.equalsIgnoreCase("unique-domains-data")) {
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val sites = Await.result(crawlerDao.findByQuery(Seq.empty, 0, 999999).map(xs => xs.map(j => j.url)), Duration.Inf)
            val filteredSites = sites.distinct.filter(s => !s.isEmpty).map(s => SiteUrl.getJustDomain(s).toLowerCase()).distinct
            val jobs = Await.result(repo.findByQuery(Seq.empty, 0, 999999).map(xs => xs.map(j => j.extras.map(e => e.originalPdf).getOrElse(""))), Duration.Inf)
            val filteredJobs = jobs.distinct.filter(s => !s.isEmpty).map(s => SiteUrl.getJustDomain(s).toLowerCase()).distinct
            val filteredJobsMap = filteredJobs.map(s => (s, s)).toMap
            val notDoneUrls = filteredSites.filter(site => {
              !filteredJobsMap.contains(site)
            }).distinct
            //Future.successful(Ok(filteredJobs.toString() + s"\nTOTAL: ${filteredJobs.size}"))
            Future.successful(Ok(notDoneUrls.toString() + s"\nTOTAL: ${notDoneUrls.size}"))
          }
          else if (name.equalsIgnoreCase("fix-site-domain")) {
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val sites = Await.result(crawlerDao.findByQuery(Seq.empty, 0, 999999), Duration.Inf)
            val filteredSites = sites.filter(s => !s.url.isEmpty).map(s => {
              val domain = SiteUrl.getJustDomain(s.url).toLowerCase()
              val comp = APP_IGNORE_PDF_FROM_SITES_ORIGINAL.contains(domain)
              val siteType = s.siteType.getOrElse(if (comp) "competitors" else "sites")
              s.copy(url = s.url.trim, originalUrl = Some(s.originalUrl.map(_.trim).getOrElse(s.url.trim)), domain=Some(domain), siteType = Some(siteType), ignore = Some("no"))
            })
            filteredSites.map(site => {
              Await.result(crawlerDao.save(site), Duration.Inf)
            })
            Future.successful(Ok("DONE"))
          }
          else if (name.equalsIgnoreCase("set-site")) {
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val linksTobefetched: List[SiteCrawler] = {
              val siteType = fetchAllParams match {
                case false => "sites"
                case _ => "competitors"
              }
              val urlList: List[String] = linksToFetch.split(",").toList
              println(s"URL to be fetched and parsed = $urlList")
              val crawlingSites = urlList.map(link => {
                val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
                SiteCrawler(name = name, url = link, domain = Some(SiteUrl.getJustDomain(link)), ignore = Some("no"), priority = priority, status = "", result = None, originalUrl = Some(link), siteType = Some(siteType))
              })
              crawlingSites.map(site => {
                val oldSites = Await.result(crawlerDao.findByQuery(Seq(("url", site.originalUrl.getOrElse(site.url))), 0, 999999), Duration.Inf)
                if (oldSites.isEmpty) Await.result(crawlerDao.save(site), Duration.Inf) else Await.result(crawlerDao.save(oldSites.head.copy(priority = priority, siteType = Some(siteType))), Duration.Inf)
              })
            }
            val sitesForFetching: List[SiteCrawler] = linksTobefetched
            println(s"SITES to be processed with user input = $sitesForFetching")
            Future.successful(Ok(sitesForFetching.toString()))
          }
          else if (name.equalsIgnoreCase("crawl-all")) {
            val crawlerDao = new SiteCrawlerDAO(db, es)
            crawlerDao.findByQuery(Seq.empty, 0, 9999).map(cx => {
              import akka.pattern.ask
              implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
              Future {
                val actorname = if (url.isEmpty) name.slug else url.slug
                val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
                ref ! StartCrawlingSites(cx.toList, cacheFolder)
              }
            })
            Future.successful(Ok("No data provided"))
          }
          else if (name.equalsIgnoreCase("crawl-all-parallel")) {
            Future.successful(Ok(fetchSingleJob(concurrencyNo, offsetNo, pageNo, priority, fetchAllParams, name, url, linksToFetch, false)))
          } else {
            Future.successful(Ok("DONE"))
          }
        case false =>
          if (name.equalsIgnoreCase("load-all-and-crawl-all")) {
            val urlList = scala.io.Source.fromFile(url).getLines().toList.map(s => s.split(",").toList).flatten.filter(s => !s.isEmpty).toList
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val crawlingSites = urlList.map(link => {
              val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
              SiteCrawler(name = name, url = link.toLowerCase.trim, domain = Some(SiteUrl.getJustDomain(link)), ignore = Some("no"), siteType = Some("sites"), priority = 1, status = "", result = None)
            })
            println(s"Total no of source urls to be crawled = ${crawlingSites.size}")
            var inew = 0
            val objects = crawlingSites.toList.zipWithIndex.map(xs => {
              val objtmp = xs._1
              val resF = crawlerDao.findByKeyValue("url", objtmp.url)
              val obj = Await.result(resF, Duration(10, TimeUnit.SECONDS))
              println(s"Checking and crawling url = ${obj}")
              val finalSite = if (obj.isDefined && obj.get._key.isDefined) {
                obj
              } else {
                inew += 1
                val r = crawlerDao.save(objtmp)
                Await.result(r, Duration(10, TimeUnit.SECONDS))
              }
            })
            println(s"No of sites saved = ${objects.size} and new urls = $inew")
          }
          else if (name.equalsIgnoreCase("download-and-cache-file")) {
            SiteParserAndCrawler.downloadViaWgetWithRetry(url, targetName = "/tmp/0.pdf", counter = 0, debug = true, forceDownload = true)
            Future.successful(Ok("Jobs migration for Skills started"))
          }
          else if (name.equalsIgnoreCase("employers_to_crawler_sites")) {
            val empDao = new EmployerDAO(db, es)
            val allEmps = Await.result(empDao.findByQuery(Seq.empty, 0, 99999), Duration.Inf)
            val allLinks = allEmps.map(e => {
              e.extra.links.getOrElse(List.empty)
            }).flatten.toList.distinct
            val urlList = allLinks.map(s => s.split(",").toList).flatten.filter(s => !s.isEmpty).toList
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val crawlingSites = urlList.map(link => {
              val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
              SiteCrawler(name = name, url = link, domain = Some(SiteUrl.getJustDomain(link).toLowerCase), ignore = Some("no"), siteType = Some("sites"), priority = 1, status = "", result = None, originalUrl = Some(link))
            })
            println(s"Total no of source urls to be crawled = ${crawlingSites.size}")
            import akka.pattern.ask
            val actorname = "load_crawling_data"
            crawlingSites.grouped(100).toList.zipWithIndex.map(xs => {
              val topindex = xs._2
              val datas: List[SiteCrawler] = xs._1.zipWithIndex.map(objidx => {
                val objtmp = objidx._1
                val resF = crawlerDao.findByKeyValue("url", objtmp.originalUrl.getOrElse(objtmp.url))
                val obj = Await.result(resF, Duration(10, TimeUnit.SECONDS)).getOrElse(objtmp)
                println(s"Checking and crawling url = ${obj}")
                val finalSite = if (obj._key.isDefined) {
                  obj
                } else {
                  val r = crawlerDao.save(obj)
                  Await.result(r, Duration(10, TimeUnit.SECONDS))
                }
                finalSite
              })
              //val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"crawl-actor-$actorname-${topindex}")
              //ref ! StartCrawlingSites(datas, cacheFolder)
            })
          }
          else if (name.equalsIgnoreCase("load_crawling_data")) {
            val urlList = scala.io.Source.fromFile(url).getLines().toList.map(s => s.split(",").toList).flatten.filter(s => !s.isEmpty).toList
            val crawlerDao = new SiteCrawlerDAO(db, es)
            val crawlingSites = urlList.map(link => {
              val name = link.replaceAll("""https://""", "").replaceAll("""http://""", "").split("/").headOption.getOrElse("")
              SiteCrawler(name = name, url = link.toLowerCase.trim, domain = Some(SiteUrl.getJustDomain(url)), ignore = Some("no"), siteType = Some("sites"),  priority = 1, status = "", result = None)
            })
            println(s"Total no of source urls to be crawled = ${crawlingSites.size}")
            import akka.pattern.ask
            val actorname = "load_crawling_data"
            crawlingSites.grouped(10).toList.zipWithIndex.map(xs => {
              val topindex = xs._2
              val datas: List[SiteCrawler] = xs._1.zipWithIndex.map(objidx => {
                val objtmp = objidx._1
                val resF = crawlerDao.findByKeyValue("url", objtmp.url)
                val obj = Await.result(resF, Duration(10, TimeUnit.SECONDS)).getOrElse(objtmp)
                println(s"Checking and crawling url = ${obj}")
                val finalSite = if (obj._key.isDefined) {
                  obj
                } else {
                  val r = crawlerDao.save(obj)
                  Await.result(r, Duration(10, TimeUnit.SECONDS))
                }
                finalSite
              })
              //val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"crawl-actor-$actorname-${topindex}")
              //ref ! StartCrawlingSites(datas, cacheFolder)
            })
          }
          else if (name.equalsIgnoreCase("crawl")) {
            val crawlerDao = new SiteCrawlerDAO(db, es)
            crawlerDao.findByKeyValue("url", url).map(c => {
              if (c.isDefined) {
                val crawler = SiteParserAndCrawler.parse(c.get.url, cacheFolder, false, 0, List.empty, List.empty, List.empty, List.empty)
                println(s"FINAL result = $crawler for url = ${c.get.url}")
              }
            })
          }
          else {
            import akka.pattern.ask
            implicit val timeout: akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
            val linesAll = scala.io.Source.fromFile(name).getLines().toList
            println(s"No of urls: ${linesAll.size}")
            Future {
              val lines = linesAll.filter(s => s.startsWith("""https"""))
              //val groupedLines = lines.grouped(1000)
              val actorname = if (url.isEmpty) name.slug else url.slug
              val ref = system.actorOf(Props(new DataLoaderActor(db, es)), s"data-loading-actor-$actorname")
              ref ! FetchAndParse(lines)
              //val resultF: Future[List[(String, String)]] = { ref ? FetchAndParse(lines._1) }.mapTo[List[(String, String)]]
              if (ref != null) {
                ref ! Stop
              }
            }
          }
          Future.successful(Ok("Started"))
      }
    })
  }

  def performIndexing(indexName: String) = Action.async { implicit request: Request[AnyContent] =>
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val action: String = app.utils.Utils.decodeQueryString(request.queryString.get("takeAction").flatMap(_.headOption).getOrElse(""))
      println(s"Performing indexing for index = $indexName")
      val empDao = new EmployerDAO(db, es)
      val result: Future[(SitePage, Seq[services.ElasticSearchResult])] = for {
        page <- sitePage.findByKeyValue("siteName", "jobs")
        emps <- if (action.equalsIgnoreCase("employers")) empDao.findByQuery(Seq.empty, 0, 99999) else Future.successful(Seq.empty[Employer])
        jobs <- {
          indexName match {
            case "jobs" =>
              val empMap: Map[String, Employer] = emps.map(e => {
                (e.website, e)
              }).toMap
              val pages = offset / 100
              println(s"Page found for index = $indexName = $page and no pages = $pages")
              Future {
                val datas: Seq[Seq[services.ElasticSearchResult]] = (1 to pages).map(p => {
                  val offset = 100
                  val dropNo = (p - 1) * offset
                  println(s"Indexing $indexName from $dropNo ")
                  val filters = if (action.equalsIgnoreCase("live")) Seq(("status","Live")) else Seq.empty
                  val datas: Future[Seq[services.ElasticSearchResult]] = repo.findByQuery(filters, dropNo, offset).map(xs => xs.toList.map(j => {
                    println(s"Indexing job key: ${j._key}")
                    val job: Job = action.equalsIgnoreCase("employers") match {
                      case true =>
                        val emp:Option[Employer] = empMap.get(j.extras.map(_.employer.map(e => e.website).getOrElse("")).getOrElse("")) match {
                          case Some(found) => Some(found)
                          case _ => None
                        }
                        if (emp.isDefined) j.copy(extras = j.extras.map(_.copy(employer = emp))) else j
                      case false => j
                    }
                    val dataJson = Json.toJson(job).toString()
                    val r = es.updateIndex(indexName, job._key.getOrElse(""), insertJson = dataJson, updateJson = dataJson)
                    println(s"Indexed job key: ${r.id}")
                    r
                  }))
                  Await.result(datas, Duration.Inf)
                })
                es.refreshIndexes(Seq("jobs"))
                datas.flatten
              }

            case "ads" =>
              val pages = offset / 100
              Future {
                val datas: Seq[Seq[services.ElasticSearchResult]] = (1 to pages).map(p => {
                  val offset = 100
                  val dropNo = (p - 1) * offset
                  println(s"Indexing $indexName from $dropNo ")
                  val datas: Future[Seq[services.ElasticSearchResult]] = new daos.FullJobDAO(db, es).findByQuery(Seq.empty, dropNo, offset).map(xs => xs.toList.map(j => {
                    println(s"Indexing $indexName key: ${j._key}")
                    val dataJson = Json.toJson(j).toString()
                    val r = es.updateIndex(indexName, j._key.getOrElse(""), insertJson = dataJson, updateJson = dataJson)
                    println(s"Indexed $indexName key: ${r.id}")
                    r
                  }))
                  Await.result(datas, Duration.Inf)
                })
                datas.flatten
              }
            case "states" =>
              val pages = offset / 100
              Future {
                val datas: Seq[Seq[services.ElasticSearchResult]] = (1 to pages).map(p => {
                  val offset = 99999
                  val dropNo = (p - 1) * offset
                  println(s"Indexing $indexName from $dropNo ")
                  val emps: Future[Seq[(String, String)]] = new EmployerDAO(db, es).findByQuery(Seq.empty, dropNo, offset).map(xs => xs.map(e => {
                    (e._key.getOrElse(""), e.extra.state)
                  }))
                  val datas: Future[Seq[services.ElasticSearchResult]] = emps.map(xs => {
                    println(s"No of employers whose state to be indexed = ${xs.size}")
                    val table = xs.map(s => (s._2, s._1)).toMap
                    xs.map(_._2).distinct.toList.filter(s => !s.trim.isEmpty).map(j => {
                      val id = table.get(j).getOrElse("")
                      val n = IDNameValue(id = Some(id), _key = Some(id), name = j, value = shared.utils.Slug.slugify(j))
                      val dataJson = Json.toJson(n).toString()
                      println(s"ID = $id, state = $n and json = $dataJson")
                      val r = es.updateIndex(indexName, id, insertJson = dataJson, updateJson = dataJson)
                      println(s"Indexed $indexName key: ${r.id}")
                      r
                    })
                  })
                  Await.result(datas, Duration.Inf)
                })
                datas.flatten
              }

            case "employers" =>
              val pages = offset / 100
              Future {
                val datas: Seq[Seq[services.ElasticSearchResult]] = (1 to pages).map(p => {
                  val offset = 100
                  val dropNo = (p - 1) * offset
                  println(s"Indexing $indexName from $dropNo ")
                  val datas: Future[Seq[services.ElasticSearchResult]] = new EmployerDAO(db, es).findByQuery(Seq.empty, dropNo, offset).map(xs => xs.toList.map(j => {
                    println(s"Indexing $indexName key: ${j._key}")
                    val dataJson = Json.toJson(j).toString()
                    val r = es.updateIndex(indexName, j._key.getOrElse(""), insertJson = dataJson, updateJson = dataJson)
                    println(s"Indexed $indexName key: ${r.id}")
                    r
                  }))
                  Await.result(datas, Duration.Inf)
                })
                datas.flatten
              }

            case _ =>
              Future.successful(Seq.empty)
          }
        }
      } yield {
        (page.getOrElse(SitePage.default), jobs)
      }
      result.map(r => {
        if (!r._2.isEmpty) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "Records are indexed"))) else
          Ok(Json.toJson(shared.ErrorResponse(code = 400, message = "Records cannot be indexed", exception = Some("ID cannot be found."))))
      })
    })
  }

  def testStart(): Unit = {

    System.setProperty("scala.concurrent.context.maxThreads", "20")
    System.setProperty("scala.concurrent.context.maxExtraThreads", "40")
    //implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

    //implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(null)

    //implicit val ec = scala.concurrent.ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor)

    //implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newWorkStealingPool(2))

    val blockingTasks = 600
    val paralleleTasks = 100

    def stage1 = {
      val futures = Vector.range(1, blockingTasks + 1).map { i =>
        Future {
          val sleepTime = 2000
          scala.concurrent.blocking {
            Thread.sleep(sleepTime)
          }

          val today = Calendar.getInstance().getTime()
          println(Thread.currentThread().getName + " Future: " + i + " - sleep was: " + sleepTime + " - " + today)
          1;
        }
      }

      val all = Future.sequence(futures)
      println("waiting on " + Thread.currentThread().getName)
      Await.result(all, Duration("100s"))
      println("finished waiting on " + Thread.currentThread().getName)
    }

    def stage2 = {
      val futures2 = Vector.range(1, paralleleTasks + 1).map { i =>
        Future {
          val sleepTime = 2000
          Thread.sleep(sleepTime)

          val today = Calendar.getInstance().getTime()
          println(Thread.currentThread().getName + " Future 2: " + i + " - sleep was: " + sleepTime + " - " + today)
          1
        }
      }

      val all2 = Future.sequence(futures2)
      println("waiting on " + Thread.currentThread().getName)
      Await.result(all2, Duration("1000s"))
      println("finished waiting on " + Thread.currentThread().getName)
      println(System.getProperty("scala.concurrent.context.maxThreads"))
      println(System.getProperty("scala.concurrent.context.maxExtraThreads"))
    }

    stage1
    stage2
  }


  def generatePage(prefix: String, title: String, suffix: String) = Action.async { implicit request: Request[AnyContent] =>
    val source: String = app.utils.Utils.decodeQueryString(request.queryString.get("source").flatMap(_.headOption).getOrElse(""))
    withAdminUserF(user => {
      val dropNo = (pageNo - 1) * offset
      val dao = new RuleDAO(db, es)
      val crawlerDao = new SiteAndPageDAO(db, es)
      val result: Future[(Seq[SitePage], Seq[SitePage])] = for {
        pages <- {
          val params: Seq[(String, String)] = Seq(("pageType", "Pages"))
            sitePage.findByQuery(params, 0, 99999)
        }
        skills <- {
          crawlerDao.findByQuery(Seq.empty, 0, 50, sortingKey = "dates.created DESC")
        }
      } yield {
        (pages, skills)
      }
      result.map(r => {
        val total = r._2.size
        val records = r._2
        implicit val pagination = Pagination(page = pageNo, offset = offset, total = total)
        Ok(boPageView(r._1.headOption.getOrElse(SitePage.default), assetsFinder, r._1.head, records, PageCondition.conditions))
      })
    })
  }


  def testAsync = Action { implicit request: Request[AnyContent] =>
    testStart()
    Ok("Started")
  }
}
