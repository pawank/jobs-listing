package actors

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import akka.event.LoggingReceive
import akka.stream.Materializer
import daos.{FullJobDAO, SiteCrawlerDAO}
import services.{ArangoDbService, ElasticsearchService}
import app.utils.FileUtils
import utils.extraction.TikaExtraction
import akka.pattern.ask
import api.{ExcelReader, JobParser, SiteParserAndCrawler}
import shared.{DownloadFile, ExecuteAndDie, Experiment, FetchAndParse, FullJob, LoadExcel, ParseFullJob, Process, RunWithSource, SourceMetadata, StartCrawlingSingleSite, StartCrawlingSites, Stop}
import shared.{DatesInfo, EducationTag, Employer, FullJob, MatchingRule}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

class DataLoaderActor(db: ArangoDbService, es: ElasticsearchService)
  extends Actor
    with ActorLogging {

  private var sourcePath: Option[String] = None
  // a flag that indicates if the process is running
  private var isRunning = false

  // a map with all clients (Websocket-Actor) that needs the status about the process
  private val userActors: mutable.Map[String, ActorRef] = mutable.Map()

  //private val datas: scala.collection.mutable.ListBuffer[FullJob] = scala.collection.mutable.ListBuffer.empty


  def receive = LoggingReceive {
    case shared.Run(path) =>
      sourcePath = Some(path)
      log.info(s"Running data loading for path: $path")
      if (isRunning) // this should not happen as the button is disabled, if running
        log.warning("The adapter is running already!")
      else {
        isRunning = true
        log.info(s"Started loading from path, $path for sender, $sender")
      }
      val source = shared.SourceMetadata(path = path)
      implicit val timeout:akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
      self ? RunWithSource(source)
      self ! Stop

    case RunWithSource(source, parsing, rid, fetchAllParams) =>
      sourcePath = Some(source.path)
      log.info(s"Running data loading for path: $sourcePath for full job id = $rid and fetchAllParams = $fetchAllParams")
      implicit val runtime = zio.Runtime.default
      val r = zio.blocking.effectBlocking {
        TikaExtraction.parseAsHtml(source.path, parsing = parsing) match {
          case Right(value) =>
            val data = FullJob(rid, rid, source = source, originalLink = None, content = Some(value), errors = if (!value.valid) Some(value.content) else None, status = Some("done"))
            //datas :+ data
            FullJobDAO.saveAll(db, es, data)
            sender ! data
          case Left(error) =>
            val data = FullJob(rid, rid, source = source, content = None, originalLink = None, errors = Some(error), status = Some("done"))
            //datas :+ data
            println(s"$error for source path = $sourcePath")
            FullJobDAO.saveAll(db, es, data)
            sender ! data
        }
      }
      runtime.unsafeRun(r)


    case ExecuteAndDie(source) =>
      sourcePath = Some(source.path)
      log.info(s"Running data loading for path: $sourcePath")
      TikaExtraction.parseAsHtml(source.path) match {
        case Right(value) =>
          val data = FullJob(None, None, source = source, originalLink = source.sourceUrl, content = Some(value), errors = if (!value.valid) Some(value.content) else None)
          //datas :+ data
          FullJobDAO.saveAll(db, es, data)
        case Left(error) =>
          val data = FullJob(None, None, source = source, content = None, originalLink = source.sourceUrl, errors = Some(error))
          //datas :+ data
          println(error)
          FullJobDAO.saveAll(db, es, data)
      }
      self ! Stop

    case Stop =>
      isRunning = false
      log.info(s"Stopped loading from path, $sourcePath")
      self ! PoisonPill

    case Process(url) =>
      implicit val timeout:akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
      FileUtils.getFilenames(url).foreach(p => {
        val source = SourceMetadata(path = p)
        self ? RunWithSource(source)
      })
      self ! Stop

    case LoadExcel(path) =>
      implicit val timeout:akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
      sender ! ExcelReader.loadEmployers(path)

    case Experiment(path) =>
      val obj = new api.AzureReadAPI()
      obj.run("/tmp/1.png", null)

    case ParseFullJob(job, emps, rules) =>
      try {
      implicit val timeout:akka.util.Timeout = Duration.apply(120, TimeUnit.SECONDS)
      val ref = new JobParser(job, emps, rules)
      val r: Tuple2[Seq[EducationTag], Seq[Employer]] = ref.parse()
      sender ! r
    } catch {
      case e: Exception =>
        e.printStackTrace()
      sender ! (Seq.empty, Seq.empty)
    }

    case FetchAndParse(urls) =>
      implicit val timeout:akka.util.Timeout = Duration.apply(12000, TimeUnit.SECONDS)
      val datas: List[(String, String)] = {
        val result = server.utils.HtmlUtils.getContentFromUrls(urls, "")
        result
      }

    case StartCrawlingSingleSite(site, cacheFolder) =>
      implicit val timeout:akka.util.Timeout = Duration.apply(120000, TimeUnit.SECONDS)
        val url = site.url
        val (error, crawler) = SiteParserAndCrawler.parse(url, cacheFolder, true, 0)
        println(s"FINAL result = ${crawler.map(_.links)} for url = ${url}")
        val obj = site.copy(result = crawler)
        SiteCrawlerDAO.saveAll(db, es, obj)
      self ! Stop

    case StartCrawlingSites(sites, cacheFolder) =>
      implicit val timeout:akka.util.Timeout = Duration.apply(120000, TimeUnit.SECONDS)
      sites.map(site => {
        val url = site.url
            val (error, crawler) = SiteParserAndCrawler.parse(url, cacheFolder, true, 0)
            //println(s"FINAL links result size = ${crawler.map(_.links).size} for url = ${url}")
            val obj = site.copy(result = crawler)
            SiteCrawlerDAO.saveAll(db, es, obj)
      })

    case DownloadFile(url, targetName) =>
      val cmd = "wget"
      val args = Seq(url,"--no-check-certificate", "-O", targetName)
      val output: Either[String, String] = utils.ShellUtils.executeViaProcessBuilder(cmd, args)
      output match {
        case Right(value) =>
          println(s"Url = $url with target name = $targetName has result = $value")
          sender() ! ""
        case Left(error) =>
          sender() ! error
      }

    case other =>
      log.info(s"unexpected message: $other")
  }
}

object DataLoaderActor {
  def props = Props[DataLoaderActor]
}
