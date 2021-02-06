package apps

import java.util.UUID
import java.util.concurrent.TimeUnit

import actors.DataLoaderActor
import akka.actor.{ActorSystem, Props}
import zio.ZIO
import zio.App
import api.{JobParser, PartnerCrawler, SiteParserAndCrawler}
import daos.{EmployerDAO, FullJobDAO, JobDAO, RuleDAO, SiteCrawlerDAO}
import play.api.mvc.{AnyContent, Request}
import services.{ArangoDbService, ElasticsearchService}
import shared.{EducationTag, Employer, FullJob, Job, MatchingRule, RunWithSource, SiteCrawler, SitePage, SourceMetadata, Stop}
import io.circe.generic.auto._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory

import scala.util.Try

object Helpers {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  //val system = ActorSystem("CmdServer")
  val db = new ArangoDbService()
  val es = new ElasticsearchService
  val config = ConfigFactory.load()
  val repo = new JobDAO(db, es)
  val crawlerDao = new SiteCrawlerDAO(db, es)
  val ruledao = new RuleDAO(db, es)
  val fulljobDao = new FullJobDAO(db, es)

  val APP_OFFSET = config.getInt("app.offset")
  val CACHE_FOLDER = {
    if ((System.getenv("APP_CACHE_FOLDER") != null) && (!System.getenv("APP_CACHE_FOLDER").isEmpty)) {
      System.getenv("APP_CACHE_FOLDER")
    } else {
      config.getString("cache.folder.name")
    }
  }
  val CACHE_FILE_PREFIX_FOLDER = config.getString("cache.folder.prefix")
  val APP_OVERRIDE_GOOGLE_PDF_FETCH = config.getString("app.override.google.fetch").equalsIgnoreCase("true")
  val APP_IGNORE_PDF_FROM_SITES = {
    val xs = config.getString("app.ignore.sites.pdf").split(",").toList
    xs ++ xs.map(site => site.replaceAll("""\.""", "-"))
  }


    def saveJob(job: Job, refLink: String): Future[Job] = {
      val r: Future[Job] = for {
        //maybeJd <- repo.findByKeyValue("sourceId", job.sourceId.getOrElse(""))
        maybeJd <- {
          val originalPdf = job.extras.map(_.originalPdf).getOrElse("None")
          repo.findByQuery(Seq(("extras.originalPdf", originalPdf), ("refLink", refLink)), 0, 1, conditionType = "OR").map(xs => xs.toList.headOption)
        }
        j <- {
          maybeJd match {
            case Some(old) =>
              if (old.status.getOrElse("").equalsIgnoreCase("Live") || old.status.getOrElse("").equalsIgnoreCase("Validated")) {
                Future.successful(old)
              } else {
                val obj = job.copy(id = old.id, _key = old._key, status = old.status)
                //println(s"Old JOB = ${old._key} found so updating the same")
                //val updateES = obj.status.getOrElse("").equalsIgnoreCase("Live")
                repo.save(obj.copy(refLink = refLink))
              }
            case _ =>
              //println(s"Old JOB not found so creating a new one")
              //val updateES = job.status.getOrElse("").equalsIgnoreCase("Live")
              repo.save(job.copy(refLink = refLink))
          }
        }
      } yield {
        println(s"JOB = ${job._key} and refLink = $refLink saved")
        j
      }
      r
    }

    def parseFullJob(obj: FullJob, employers: Seq[Employer], matches: Seq[MatchingRule], crawler: Option[SiteCrawler]): Future[Either[String, Seq[Job]]] = {
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
          val xs: Tuple2[Seq[EducationTag], Seq[Employer]] = ref.parse()
          val simpleJob = PartnerCrawler.siteCrawlerToEnhancedSimpleJon(obj.source.url.getOrElse(""), crawler)
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
      }
      resultFF
    }
    val APP_BASE_HTTP_URL = {
      val url = System.getenv("APP_BASE_HTTP_URL")
      if (url == null) "" else url
    }
    def helperProcessSingleAd(fulljob: FullJob, crawler: Option[SiteCrawler]): Future[List[Job]] = {
      val sampleCount = 0
      val adRepo = new FullJobDAO(db, es)
      val result = for {
        jobs <- {
          val params = Seq(("source.url", fulljob.source.url.getOrElse("")))
          //val params = Seq(("status", "done"), ("source.url", fulljob.source.url.getOrElse("")))
          adRepo.findByQuery(params, 0, if (sampleCount > 0) sampleCount else 99999)
        }
        matches <- new RuleDAO(db, es).findByQuery(Seq.empty, 0, 99999)
        emps <- new EmployerDAO(db, es).findByQuery(Seq.empty, 0, 99999)
      } yield {
        (jobs.toList, matches, emps)
      }
      val records = Await.result(result, Duration.Inf)
      val jobs = records._1
      val jdsSize = jobs.size
      val emps = records._3
      val matches = records._2
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
                    //val link = routes.SiteAndPageController.getFileByTagAndId("ads", , ).absoluteURL()
                    val id = jd._key.getOrElse("")
                    val name = jd.content.map(_.name).getOrElse("")
                    val link = s"$APP_BASE_HTTP_URL/backoffice/files/ads/$id/$name"
                    println(s"Job created for url = ${jd.source.url}")
                    Await.result(saveJob(j, link), Duration.Inf)
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
}
