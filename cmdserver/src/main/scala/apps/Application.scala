package apps

import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.TimeUnit

import actors.DataLoaderActor
import akka.actor.{ActorSystem, Props}
import zio.ZIO
import zio.App
import api.{JobParser, PartnerCrawler, SiteParserAndCrawler}
import apps.Application.processCrawlingToJobs
import daos.{EmployerDAO, FullJobDAO, JobDAO, RuleDAO, SiteCrawlerDAO}
import play.api.mvc.{AnyContent, Request}
import services.{ArangoDbService, ElasticsearchService}
import shared.{DatesInfo, EducationTag, Employer, FullJob, Job, MatchingRule, RunWithSource, SiteCrawler, SitePage, SiteUrl, SourceMetadata, Stop}
import io.circe.generic.auto._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory

import scala.util.Try
import shared.KeyID

object Application extends App {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val system = ActorSystem("CmdServer")
  val db = new ArangoDbService()
  val es = new ElasticsearchService
  val config = ConfigFactory.load()
  val repo = new JobDAO(db, es)
  val crawlerDao = new SiteCrawlerDAO(db, es)
  val ruledao = new RuleDAO(db, es)
  val fulljobDao = new FullJobDAO(db, es)
  implicit val runtime = zio.Runtime.default

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

  def helperProcessCrawlingToJobs(concurrencyNo: Int, offsetNo: Int, pageNo: Int, name: String, url: String, parsing: Boolean, fetchCompsFirst: String, filterCondition: String, siteID: String, isDownloadNow: Boolean) = {
    println(s"Starting processing for crawling and job creation: $name, page no = $pageNo and offset = $offsetNo with concurrency =  $concurrencyNo and siteID = $siteID for provided url: $url")
    import shared.utils.Slug._
    val cacheFolder = CACHE_FOLDER
    //val datas = crawlerDao.findByQuery(Seq.empty, 0, offsetNo)
    val siteq: Seq[(String, String)] = {
      val q = fetchCompsFirst match {
      case "competitors" =>
        Seq(("siteType", "competitors"), ("ignore", "no"))
      case "sites" =>
        Seq(("siteType", "sites"), ("ignore", "no"))
      case _ =>
        Seq(("ignore", "no"))
    }
      if (url.isEmpty) q else Seq(("domain", url))
  }
      val (fieldToCheck: String, valueToMatch: String) = filterCondition match {
        case "" => 
          ("", "")
        case "all" => 
          ("", "")
        case _ =>
          val tokens = filterCondition.split("=")
          (tokens(0), tokens(1))
      }
    val startingfinalResultF = for {
      datas <- {
        println(s"Using Q for crawler sites = $siteq for url = $url")
        siteID match {
          case "0" =>
            crawlerDao.findByQuery(siteq, 0, 999999, conditionType = "AND", sortingKey = "priority DESC")
          case _ =>
            crawlerDao.findByID(KeyID(_key = Some(siteID), id = None)).map(opt => {
              println(s"Found site for id = $siteID == $opt")
              if (opt.isDefined) Seq(opt.get) else Seq.empty
            })
        }
      }
      records <- {
        println(s"No of records found using Q = $siteq == ${datas.size}")
        val records = datas.filter(r => {
        siteID match {
          case "0" =>
          val case1 = r.status.equalsIgnoreCase("") || r.status.equalsIgnoreCase("pending")
          val case2 = fieldToCheck match {
            case "name" =>
              r.name.toLowerCase.startsWith(valueToMatch.toLowerCase())
            case "priority" =>
              r.priority >= valueToMatch.toInt
            case _ => true 
          }
          println(s"Record match case1 = $case1 and case2 = $case2 for url = ${r.url} with final filter match = ${case1 && case2}")
          case1 && case2
          case _ =>
            true
        }
        }).drop(pageNo * offsetNo).take(offsetNo)
        println(s"Starting with total no of records = ${records.size} for offset no = $offsetNo and q = $siteq")
        Future.successful(records)
      }
    } yield records

    def helperSiteCrawelerToFullJob(site: SiteCrawler, parsing: Boolean, isReParsing: Boolean = false): scala.concurrent.Future[Seq[FullJob]] = {
      val myExecutionContext: ExecutionContext = system.dispatchers.lookup("portal-context")
      val cacheFolder = CACHE_FOLDER
      try {
        val dao = new SiteCrawlerDAO(db, es)
        val params = Seq(("url", site.url))
        val output: Future[Seq[FullJob]] = dao.findByQuery(params, 0, 99999).map(datas => {
          val files: List[SourceMetadata] = datas.map(s => 
                SiteCrawler.toSourceMetadataList(s)
            /*
          s.result.map(ss => ss.paths.zipWithIndex.map(c => {
            val link = if (ss.links.size >= ss.paths.size) ss.links(c._2) else ""
            val s3path = if (ss.s3paths.size >= ss.s3paths.size) ss.s3paths(c._2) else ""
            val name = if (ss.names.size >= ss.names.size) ss.names(c._2) else ""
            val path = if (ss.paths.size >= ss.paths.size) ss.paths(c._2) else ""
            val label = if (ss.labels.size >= ss.paths.size) ss.labels(c._2) else ""
            val publicUrl = if (!s3path.isEmpty) Some(s3path) else Some(link)
            SourceMetadata(path = c._1, url = Some(link), sourceUrl = Some(ss.sourceUrl), title = Some(label), downloadPath = Some(path), s3Path = Some(s3path), publicUrl = publicUrl, negativeMatches = ss.negativeMatches, positiveMatches = ss.positiveMatches)
          })
          ).getOrElse(Seq.empty)
          */
          ).flatten.toList.filter(s => !s.url.getOrElse("").isEmpty)
          import akka.pattern.ask
          //implicit val timeout: akka.util.Timeout = Duration.Inf
          implicit val timeout: akka.util.Timeout = Duration.apply(360000, TimeUnit.SECONDS)
          val finalfiles = files.filter(s => !s.path.isEmpty)
          println(s"Got urls to be processed for FullJob = ${finalfiles.size} for url = ${site.url}")
          val dataxs:Seq[FullJob] = {
            val fullJobsMap: Map[String, FullJob] = {
              val rawUrls = finalfiles.map(_.url.getOrElse("")).filter(!_.isEmpty())
              val raws = Await.result(fulljobDao.findByQuery(Seq(("source.url", rawUrls.mkString(","))), 0, 99999, conditionType = "IN"), Duration(120, TimeUnit.SECONDS))
              raws.map(r => (r.source.url.getOrElse(""), r)).toMap
            }
            val tmpRawJobs: Seq[Option[FullJob]] = finalfiles.zipWithIndex.map(xs => {
              try {
                val i = xs._2
                val p = xs._1
                //val fulljobR = fulljobDao.findByKeyValue("source.url", p.url.getOrElse(""))
                //val fulljob = Await.result(fulljobR, Duration(90, TimeUnit.SECONDS))
                val fulljob = fullJobsMap.get(p.url.getOrElse(""))
                val rid = fulljob.map(_._key.getOrElse(""))
                if (fulljob.isDefined) {
                  println(s"helperSiteCrawelerToFullJob: Path already processed for FullJob = ${p.path}")
                  fulljob
                } else {
                  val pid = shared.utils.Slug.slugify(p.url.getOrElse(""))
                  val uuidValue = UUID.randomUUID().toString
                  val actorname = s"actor-${pid}-$uuidValue"
                  val ref = system.actorOf(Props(new DataLoaderActor(db, es)), actorname)
                  val source = p
                  println(s"Started actor $actorname for path = ${source.path}")
                  val r = ref ? RunWithSource(source, parsing, id = rid)
                  app.utils.Utils.writeLog("/tmp/cache_processed.txt", source.path, true)
                  val fulljob: FullJob = Await.result(r.mapTo[FullJob], scala.concurrent.duration.Duration.Inf)
                  println(s"Stopped actor $actorname for path = ${source.path}")
                  //println(s"Stopped actor $actorname for path = ${source.path} with result = $fulljob")
                  ref ! Stop
                  Some(fulljob)
                }
              }catch {
                case e: Exception =>
                val error = app.utils.Utils.stackTrace(e)
                  println(s"\nERROR for processing source file = $xs == $error")
                  println(s"\nERROR for processing above source file = ${xs._1.url}")
                  None
              }
              //}(myExecutionContext)
            })
            tmpRawJobs.filter(_.isDefined).map(_.get)
          }
          dataxs
        })
        output
      }catch {
        case e: Exception =>
          e.printStackTrace()
          Future.successful(Seq.empty[FullJob])
      }
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
            val sourceUrl = obj.source.url.getOrElse("")
            var alreadymsg = ""
            val isContinue: Boolean = {
              val job = Await.result(repo.findByKeyValue("extras.originalPdf", sourceUrl), Duration.Inf)
              if (job.isDefined) {
              val jjj = job.get
              println(s"parseFullJob: Found old Job with id = ${jjj._key} and title = ${jjj.title}")
              val isLive = jjj.status.getOrElse("").equalsIgnoreCase("Live")
              val isIgnored = jjj.status.getOrElse("").equalsIgnoreCase("Ignored")
              if (isLive || isIgnored) {
                  val jid = jjj._key.getOrElse("")
                  alreadymsg = s"Fetch Status: Already found with status LIVE / IGNORED ($isIgnored) with JID = ${jid} and url = $sourceUrl"
                  println(alreadymsg)
                  false
              } else true
            } else true
          }
          if (isContinue) {
            implicit val timeout: akka.util.Timeout = Duration.apply(1200, TimeUnit.SECONDS)
            val ref = new JobParser(obj, employers, matches)
            val simpleJob = PartnerCrawler.siteCrawlerToEnhancedSimpleJon(obj.source.url.getOrElse(""), crawler)
            val xs: Tuple2[Seq[EducationTag], Seq[Employer]] = {
              val employerDAO = new EmployerDAO(db, es)
              val foundLinks: Seq[SiteUrl] = simpleJob.map(_.links.getOrElse(Seq.empty)).getOrElse(Seq.empty).filter(e => e.whatFor.contains("website") || e.whatFor.contains("apply"))
              val empUrl = foundLinks.headOption.map(_.url).getOrElse("")
              val empDomain = SiteUrl.getJustDomain(empUrl)
              val maybeEmp = Await.result(employerDAO.findByQuery(Seq(("domain", empDomain)), 0, 1), Duration.Inf)
              val r = ref.parse()
              if (!maybeEmp.isEmpty) (r._1, maybeEmp) else r
            }
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
          } else {
            Left(alreadymsg)
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
                    Await.result(apps.Helpers.saveJob(j, link), Duration.Inf)
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


    def process(records: Seq[SiteCrawler]) = {
      println(s"Starting to run process()")
      val finalResultF = for {
        urlRulesNegatives <- ruledao.findByQuery(Seq(("rule", "negative_keywords")), 0, 99999, conditionType = "OR")
        titleRulesNegatives <- ruledao.findByQuery(Seq(("rule", "negative_keywords")), 0, 99999, conditionType = "OR")
        jobUrlRulesPositives <- ruledao.findByQuery(Seq(("rule", "positive_keywords")), 0, 99999, conditionType = "OR")
        jobTitleRulesPositives <- ruledao.findByQuery(Seq(("rule", "positive_keywords")), 0, 99999, conditionType = "OR")
        //datas <- crawlerDao.findByQuery(Seq(("status", "")), 0, 999999, conditionType = "OR")
        recordsF <- {
          println(s"process: Starting with total no of records = ${records.size} for offset no = $offsetNo")
          val negativeUrlKeywords: List[String] = urlRulesNegatives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim)
          val negativeTitleKeywords: List[String] = titleRulesNegatives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim)
          val positiveUrlKeywords: List[String] = jobUrlRulesPositives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim)
          val positiveTitleKeywords: List[String] = jobTitleRulesPositives.map(s => s.matches).flatten.toList.filter(s => !s.trim.isEmpty).map(s => s.trim)
          val nsize = negativeUrlKeywords.size
          Future {
            records.map(cx => {
              val sites = List(cx)
              try {
                val xs: List[SiteCrawler] = sites.map(site => {
                  val url = site.url
                  println(s"Starting to get parsed data for url = ${url} with negative url keywords size = ${nsize}")
                  //val crawler = SiteParserAndCrawler.parse(url, cacheFolder, parsing, 0, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords)
                  val (crawlerError, crawler) = if (site.siteType.getOrElse("").equalsIgnoreCase("competitors"))
                    PartnerCrawler.parse(url, cacheFolder, if (isDownloadNow) isDownloadNow else parsing, 0, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords)
                  else
                    SiteParserAndCrawler.parse(url, cacheFolder, if (isDownloadNow) isDownloadNow else parsing, 0, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords)
                  println(s"FINAL links result size = ${crawler.map(_.links).size} for url = ${url}")
                  val obj = site.copy(result = crawler, status = "crawled", output = Some(crawlerError), ignore = Some(if (!crawlerError.isEmpty) "yes" else "no"))
                  //val jobsF: Future[(List[Job], SiteCrawler)] = for {
                  val jobsF: Future[SiteCrawler] = for {
                    crawleddata <- crawlerDao.save(obj)
                    fulljobs <- {
                      println(s"Starting to get Ads for crawled site data = ${crawleddata.url}")
                      helperSiteCrawelerToFullJob(crawleddata, parsing)
                    }
                    jobsAndSite <- {
                      println(s"No of FullJob to be processed for url = ${url} == ${fulljobs.size}")
                      val rrr = for {
                        _xs <- {
                          val jobsF: Seq[Future[List[Job]]] = fulljobs.map(fulljd => {
                            println(s"FullJob is to be processed for url = ${url} == ${fulljd._key}")
                            val _result: Future[List[Job]] = helperProcessSingleAd(fulljd, Some(obj))
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
                  Await.result(jobsF, Duration.Inf)
                  //Await.result(jobsF, Duration(600, TimeUnit.SECONDS))
                })
                println(s"Ended process SiteCrawler objs = ${records.size}")
                xs
              } catch {
                case e: Exception =>
                  val error = app.utils.Utils.stackTrace(e)
                  println(s"\n\nERROR for site craweler = $cx == $error\n\n")
                  List.empty
              }
            })
          }
        }
      } yield {
        //recordsF.flatten.toList
        recordsF
      }
      finalResultF
    }

    /*
    val allTasksF = finalResultF.map(tasks => tasks.map(t => zio.Task.effect(process(Seq(t)))))
    println(s"Running concurrent tasks with concurrency no = $concurrencyNo and offsetNo = $offsetNo")
    implicit val runtime = zio.Runtime.default
    allTasksF.map(allTasks => {
      val tasks = zio.Task.collectAllParN(concurrencyNo)(allTasks)
      runtime.unsafeRun(tasks)
    })
     */
    val sites = Await.result(startingfinalResultF, Duration.Inf)
    println(s"No of sites found for process = ${sites.size} for filterCondition = $filterCondition")
    val xs = sites.map(t => process(Seq(t)))
    Await.result(Future.sequence(xs), Duration.Inf)
  }

  def processCrawlingToJobs(concurrencyNo: Int, offsetNo: Int, pageNo: Int, name: String, url: String, parsing: Boolean, fetchCompsFirst: String, filterCondition: String, siteID: String, isDownloadNow: Boolean) = {
    /*
    import zio.duration._
    val fixedZioSch:zio.Schedule[zio.clock.Clock with zio.console.Console,Any,Int] = zio.Schedule.fixed(300.second)
    implicit val runtime = zio.Runtime.default
    val schedEff = for {
      s <- zio.Task.effect(helperProcessCrawlingToJobs(concurrencyNo, offsetNo, pageNo, name, url, parsing, fetchCompsFirst, filterCondition)).repeat(fixedZioSch)
      _ <- zio.console.putStrLn(s"Process crawling to jobs zio result = $s")
    } yield ()
    runtime.unsafeRunAsync_(schedEff)
    */
    helperProcessCrawlingToJobs(concurrencyNo, offsetNo, pageNo, name, url, parsing, fetchCompsFirst, filterCondition, siteID, isDownloadNow = isDownloadNow)
  }

  def resetSitesForCrawling(reset: String, whichCase: String, siteID: String): Seq[SiteCrawler] = {
    if (siteID.equalsIgnoreCase("0")) {

    val siteq: Seq[(String, String)] = whichCase match {
      case "competitors" =>
        Seq(("siteType", "competitors"))
      case "sites" =>
        Seq(("siteType", "sites"))
      case _ =>
        Seq.empty
    }
    val finalResultF: Future[Seq[SiteCrawler]] = for {
      updateStatus <- {
        if (reset.equalsIgnoreCase("reset") || reset.equalsIgnoreCase("resetall")) {
          val started = DatesInfo.getCurrentDatesInUTC().created
          val f1 = crawlerDao.batchUpdate(siteq, "AND", ("fullJobStatus", ""))
          Await.result(f1, Duration.Inf)
          val f2 = crawlerDao.batchUpdate(siteq, "AND", ("jobStatus", ""))
          Await.result(f2, Duration.Inf)
          println(s"Reset done for SiteCrawler objects using Q = $siteq")
          crawlerDao.batchUpdate(siteq, "AND", ("status", ""))
        } else Future.successful(true)
      }
      jobupdateStatus <- {
        val fullReset = whichCase match {
          case "competitors" =>
            false
          case "sites" =>
            false
          case _ =>
            true
        }
        if (reset.equalsIgnoreCase("resetall")) {
          println(s"Reset and full reset done for Job objects")
          repo.batchUpdate(Seq.empty, "AND", ("fetchStatus", "Pending"))
         } else Future.successful(true)
      }
      datas <- crawlerDao.findByQuery(siteq, 0, 999999, conditionType = "OR")
      records <- {
        /*
        Future.sequence(datas.map(d => {
          crawlerDao.save(d.copy(status = ""))
        }))*/
        Future.successful(datas)
      }
    } yield records
    val recs = Await.result(finalResultF, Duration.Inf)
    val cnt = recs.size
    println(s"Site crawler objects reset done for count = ${cnt}")
    //cnt
    recs
  } else {
    println(s"Nothing to be reset because site ID = $siteID")
    Seq.empty
  }
  }
  
  
  def setJobsExpiry(): Int = {
    val currentDate = app.utils.Utils.getCurrentZonedDateInUTC()
    val finalResultF = for {
      jobs <- {
        repo.findByQuery(Seq(("status", "Live")), 0, 9999999)
      }
      records <- {
        Future {
          jobs.filter(job => {
            app.utils.Utils.isExpired(job.endDate.map(app.utils.Utils.localToZonedDateInUTC(_)).getOrElse(currentDate), currentDate)
          })
        }
      }
      expiredList <- {
        val xs = records.map(job => {
            repo.save(job.copy(status = Some("LiveExpired")))
        })
        Future.sequence(xs)
      }
    } yield expiredList
    val jobs = Await.result(finalResultF, Duration.Inf)
    println(jobs.map(_._key))
    println(s"No of jobs expired: ${jobs.size}")
    jobs.size
  }


  //run 10 50 0 crawl-all-parallel "" true
  //For local: run 5 10 0 crawl-all-parallel "" true
  def run(args: List[String]) = {
    println(s"Cmd server args: ${args}")
    val no = 0
    if (args.size <= 5) {
      println(s"run [concurrency factor] [offset no] [max page no] [action name] [reset?] [parsing?] [competitors/sites] [filter condition:name/priority]\n\n")
          val arg0 = args(no)
          arg0 match {
            case "set-jobs-expiry" =>
              val task = zio.Task.effectTotal({
                  setJobsExpiry()
              })
              runtime.unsafeRun(task)
            case _ =>
          }
    } else {
      val concurrencyNo: Int = args(no).toInt
      val offsetNo: Int = args(no+1).toInt
      val pageNo: Int = args(no+2).toInt
      val name: String = args(no+3)
      val reset: String = args(no+4)
      val parsing: Boolean = args(no+5).toBoolean
      val fetchCompsFirst : String = args(no+6)
      val filterCondition: String = args(no+7)
      val siteID: String = args(no+8)
      val isDownloadNow: Boolean = args(no+9).toBoolean
      //val crawlerDao = new SiteCrawlerDAO(db, es)
      //val finalResultF = crawlerDao.findByQuery(Seq.empty, 0, 999999, conditionType = "OR")
      //val recs = Await.result(finalResultF, Duration.Inf)
      //println(s"No of records found = ${recs.size}")
      //val totalRecords = resetSitesForCrawling(reset, fetchCompsFirst, siteID)
      val toBeProcessedRecords = resetSitesForCrawling(reset, fetchCompsFirst, siteID)
      val totalRecords = toBeProcessedRecords.size
      val totalPages = (totalRecords / offsetNo) + 1
      println(s"Total records from reset sites if needed = $totalRecords and offset limit = $offsetNo with total max page no = $totalPages")
      val tobeParsedUrls: Seq[String] = try {
          scala.io.Source.fromFile("/tmp/data_tobe_fetched.txt").getLines().toList.map(s => s.split(",").toList).flatten.map(_.trim)
        } catch {
          case e: Throwable =>
              println(s"/tmp/data_tobe_fetched.txt Not found")
              Seq.empty
        }
      val tobeParsedUrlsMap:Map[String, String] = {
        if (!tobeParsedUrls.isEmpty) tobeParsedUrls.map(s => (s, s)).toMap else toBeProcessedRecords.map(s => (s.url, s.url)).toMap
      }
      val finalRecordsForProcessing: Seq[String] = if (!tobeParsedUrls.isEmpty) tobeParsedUrls else toBeProcessedRecords.map(s => s.url)
      val sizetobeParsedUrls = {
          val sz = tobeParsedUrls.size
          if (sz > 0) sz else -1
      }
      val fixedPageNo = 0
      var counter = 0
      finalRecordsForProcessing.zipWithIndex.map(url => {
        val i = url._2 + 1
        println(s"Starting counter = $i with offset = $offsetNo and concurrency value = $concurrencyNo for action = $name with parsing = $parsing for url = ${url._1}")
        if (sizetobeParsedUrls > 0)
          println(s"Size of to be done urls == ${sizetobeParsedUrls}, hence, only urls from the file will be fetched. Removed /tmp/data_tobe_fetched to fetch from db.\n\n")
        Try {
          //import zio.duration._
          val task = zio.Task.effectTotal({
            val presult = processCrawlingToJobs(concurrencyNo, offsetNo, pageNo = fixedPageNo
              , name = name
              , url = url._1
              , parsing = parsing
              , fetchCompsFirst = fetchCompsFirst
              , filterCondition = filterCondition
              , siteID = siteID
              , isDownloadNow = isDownloadNow
            )
            presult
          })
          runtime.unsafeRun(task)
        }
        println(s"Done for counter = ${url._2} with offset = $offsetNo and concurrency value = $concurrencyNo for action = $name with parsing = $parsing for url = ${url._1}")
        counter += 1
      })
      println(s"Total no of records finally processed = ${counter} and all done ($counter out of ${totalRecords}) == ${totalRecords == counter}")
      println(s"PROGRAM FINISHED. Press Ctrl+C to close now.")
      System.exit(0)
    }
    myAppLogic.exitCode
  }

  val myAppLogic = for {
    obj <-  ZIO.fromOption(Some(true))
    } yield {
      ()
    }
}
