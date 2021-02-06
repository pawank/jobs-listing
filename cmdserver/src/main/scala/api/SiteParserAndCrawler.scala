package api

import java.io.File
import java.net.InetSocketAddress

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import os.Path
import shared.SiteCrawlerResult
import utils.ShellUtils

import scala.jdk.CollectionConverters._
import scala.util.Try

object SiteParserAndCrawler {
  implicit val runtime = zio.Runtime.default
  //val READ_TIMEOUT          = 12000
  //val CONNECT_TIMEOUT       = 5000
  val READ_TIMEOUT = {
    val x = System.getenv("READ_TIMEOUT")
    if (x == null) {
      1000 * 120
    } else {
      1000 * x.toInt
    }
  }
  val CONNECT_TIMEOUT = {
    val x = System.getenv("CONNECT_TIMEOUT")
    if (x == null) {
      1000 * 10
    } else {
      1000 * x.toInt
    }
  }
  val TEXT_LIMIT = 1000
  val NO_OF_PAGES_FOR_MATCH = 2

  import java.io.IOException

  def pingHost(host: String, port: Int, timeoutInMS: Int): Boolean = {
    import java.net.Socket
    var isTimeout = false
    var socket: Socket = null;
    var status = false
    try {
      socket = new Socket()
      println(s"Ping host = $host:$port with timeout = $timeoutInMS ms")
      socket.connect(new InetSocketAddress(host, port), timeoutInMS)
      println(s"Ping host = $host:$port with timeout = $timeoutInMS ms DONE")
      status = true
    } catch {
      case e: IOException =>
        //e.printStackTrace()
        val error = app.utils.Utils.stackTrace(e)
        isTimeout =  error.contains("java.net.SocketTimeoutException: connect timed out")
        false // Either timeout or unreachable or failed DNS lookup.
    } finally {
      if (socket != null) socket.close()
    }
    if (isTimeout) true else status
  }

  def isUrlAlive(url: String): Boolean = {
    val prefixUrl = url
      .replaceAll("""https://""", "")
      .replaceAll("""http://""", "")
      .split("/")
      .toList
      .headOption
      .getOrElse("")
    if (prefixUrl.contains(":")) {
      val tokens = prefixUrl.split(":")
      pingHost(tokens(0), tokens(1).toInt, 5000)
    } else {
      if (url.contains("https://")) pingHost(prefixUrl, 443, 5000) else pingHost(prefixUrl, 80, 5000)
    }
  }

  def isNegativeKeywordFound(url: String, negativeKeywords: List[String]): String = {
    var found = ""
    negativeKeywords.foldLeft(false)((a, b) => {
      val m1 = (!b.toLowerCase.isEmpty) && (!url.toLowerCase.isEmpty)
      val m = url.toLowerCase.contains(b.toLowerCase)
      val v = a || (m1 && m)
      //println(s"NEG url = $url, a = $a, b = $b, m1 = $m1 and match = $m v = $v")
      if (v) {
        found = b
        false
      } else v
    })
    //println(s"Negative keyword found = $found")
    found
  }

  def isNegativeUrlFound(url: String, negativeKeywords: List[String]): String = isNegativeKeywordFound(url, negativeKeywords)

  def isPositiveUrlFound(url: String, keywords: List[String]): String = {
  var found = ""
  keywords.foldLeft(false)((a, b) => {
    val m1 = (!b.toLowerCase.isEmpty) && (!url.toLowerCase.isEmpty)
    val m = url.toLowerCase.contains(b.toLowerCase)
    val v = a || (m1 && m)
    //println(s"POS url = $url, a = $a, b = $b, m1 = $m1 and match = $m v = $v")
    if (v) {
      found = b
      false
    } else v
  })
    //println(s"Positive keyword found = $found")
  found
}

  def isPositiveKeywordFound(title: String, keywords: List[String]): String = isPositiveUrlFound(title, keywords)

  def isFileExistsAtPath(path: String): Boolean = {
    import java.nio.file.{Paths, Files}
    Files.exists(Paths.get(path))
  }

  //Ex url http://cimfr.nic.in/upload_files/current_opportunity/1590495877_Final%20Advt.%2002-2020.pdf
  //url https://cdn.s3waas.gov.in/s39c82c7143c102b71c593d98d96093fde/uploads/2020/06/20200623100.pdf
  def downloadAndCacheUrlToFile(url: String, targetName: String, counter: Int = 0, debug: Boolean = false, forceDownload: Boolean = false): (String, String) = {
    try {
      if ((!forceDownload) && isFileExistsAtPath(targetName)) {
        if (debug)
          println(s"Target file at $targetName for url = $url already exists")
        (targetName, url)
      } else {
        if (debug)
          println(s"Target file at $targetName for url = $url download starting")
        if (isUrlAlive(url)) {
          val cmd = "wget"
          //10 seconds --dns-timeout=, --connect-timeout, and --read-timeout=
          val args = if (url.toLowerCase.startsWith("https"))
            Seq(url,"--load-cookies=cookies.txt", "--no-check-certificate","--max-redirect", "--no-verbose", "--waitretry=0", "--retry-connrefused", "--quiet", "--timeout=10",  "--tries=2", "--user-agent=\"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36\"", "-O", targetName)
          else
            Seq(url,"--load-cookies=cookies.txt", "--max-redirect", "--no-verbose", "--waitretry=0", "--retry-connrefused", "--quiet", "--timeout=10",  "--tries=2", "--user-agent=\"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36\"", "-O", targetName)
          val output: Either[String, String] = utils.ShellUtils.executeViaProcessBuilder(cmd, args, debug = debug)
          output match {
            case Right(value) =>
              //println(s"Url = $url with target name = $targetName has result = $value")
              println(s"Url = $url with target name = $targetName via wget")
              (targetName, url)
            case Left(error) =>
              println(s"ERROR: Download for url = $url and file path = $targetName as $error")
              ("", url)
          }
        } else {
          if (debug)
            println(s"Target file at $targetName for url = $url download stopped because url is inactive")
          ("", url)
        }
      }
    } catch {
      case e: Throwable =>
        val error = app.utils.Utils.stackTrace(e)
        if (debug)
          println(s"Error in downloading file via wget at $targetName with error = $error from url: $url")
        else
          println(s"Error in downloading file via wget at $targetName from url: $url")
        if (counter >= 1) {
          var finalUrl = url
          Try {
            val status = utils.ShellUtils.fileDownloader(url, targetName, CONNECT_TIMEOUT, READ_TIMEOUT, allowRedirect = false)
            if (debug)
              println(s"Status = $status for url download = $url")
            val lines = scala.io.Source.fromFile(targetName).getLines().toList
            val data = lines.mkString(",")
            if (data.contains("301 Moved Permanently") && data.contains("Location:")) {
              lines.foreach(line => {
                if (line.contains("Location:")) {
                  val tokens = line.split("Location:").toList
                  val newUrl = tokens(1).trim
                  println(s"NEW URL = $newUrl")
                  finalUrl = newUrl
                }
              })
            }
          }
          if (finalUrl.equalsIgnoreCase(url)) {
            (targetName, finalUrl)
          } else {
            val ext = finalUrl.toLowerCase.split("""\.""").reverse.head
            val finalTarget = s"${targetName}_${counter}.$ext"
            val status = utils.ShellUtils.fileDownloader(finalUrl, finalTarget, CONNECT_TIMEOUT, READ_TIMEOUT, allowRedirect = true)
            (finalTarget, finalUrl)
            //downloadAndCacheUrlToFile(finalUrl, finalTarget, 1, debug = debug)
          }
        } else {
          if (counter <= 0) {
            downloadAndCacheUrlToFile(ShellUtils.changeHttpHttps(url), targetName, counter + 1, debug, forceDownload)
          } else {
            println(s"Error in downloading file via wget at $targetName from url: $url with counter = $counter == $error")
            ("", url)
          }
        }
    }
  }

  def downloadViaWgetWithRetry(url: String, targetName: String, counter: Int = 0, debug: Boolean = false, forceDownload: Boolean = false): String= {
      val status: (String, String) = downloadAndCacheUrlToFile(url, targetName, counter, debug, forceDownload)
    status._1
  }

  def downloadUrlToFile(url: String, targetName: String): String = {
    //import scala.language.postfixOps
    def downloadFile(encUrl: String): String =
      //import sys.process._
      //import java.net.URL
      //import java.io.File
      try {

        val output: Either[String, String] = {
          //val result: String = { new URL(encUrl) #> new File(targetName) !! }
          //val result = requests.get(encUrl, verifySslCerts = false)
          val result = requests.get(encUrl, readTimeout = READ_TIMEOUT, connectTimeout = CONNECT_TIMEOUT, verifySslCerts = false)
          app.utils.Utils.writeToFile(result.contents, targetFile = targetName)
          //println(s"Saved to file = $targetName with result = $result")
          Right("")
        }
        output match {
          case Right(value) =>
            //println(s"Url = $url with target name = $targetName has result = $value")
            targetName
          case Left(error) =>
            println(s"ERROR: Download for url = $url and file path = $targetName as $error")
            ""
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          val error = app.utils.Utils.stackTrace(e)
          println(s"Error found = $error from url: $url")
          ""
      }
    /*
    val zUrl = zio.ZIO.fromEither(app.utils.Utils.encodeUrl(url))
    val r: zio.ZIO[Any,String,String] = for {
      encUrl <- zUrl
      targetFile <- zio.Task.succeed(downloadFile(encUrl))
    } yield targetFile
     */
    /*
    val encUrlR = app.utils.Utils.encodeUrl(url)
    encUrlR match {
      case Right(encUrl) =>
          downloadFile(encUrl)
      case Left(e) =>
          ""
    }
     */
    downloadFile(url)
  }

  def makeCompleteUrls(prefixUrl: String, prefixHttp: String, urls: Seq[(String, String)]): Seq[(String, String)] = {
    urls.map { linkText =>
      val link = linkText._1
      val isRelativeLink = !(link.toLowerCase.startsWith("https://") || link.toLowerCase.startsWith(
        "http://"
      ) || link.toLowerCase.startsWith("www"))
      val linkUrl = if (link.toLowerCase.startsWith("/") || isRelativeLink) {
        if (isRelativeLink) {
          prefixHttp + prefixUrl + "/" + link
        } else {
          prefixHttp + prefixUrl + link
        }
      } else link
      (linkUrl, linkText._2)
    }
  }

  def fetchHtmlContent(url: String): String = {
    if (!isUrlAlive(url)) {
      ""
    } else {
      val params = Map()
      //val r = requests.get(url, params = params, verifySslCerts = false)
      println(s"Fetching url via requests for potential PDF links from url = $url")
      val r = requests.get(url, params = params, readTimeout = READ_TIMEOUT, connectTimeout = CONNECT_TIMEOUT, verifySslCerts = false)
      //println(r)
      println(s"Found status code == ${r.statusCode} for html content from pdf for url = $url")
      r.statusCode match {
        case 200 =>
          val content = r.text()
          content
        case _ =>
          ""
      }
    }
  }


  def fetchUrlAndReturnLinks(url: String): Seq[(String, String)] = {
    if (!isUrlAlive(url)) {
      Seq.empty
    } else {
      val params = Map()
      //val r = requests.get(url, params = params, verifySslCerts = false)
      println(s"Fetching url via requests for potential PDF links from url = $url")
      val r = requests.get(url, params = params, readTimeout = READ_TIMEOUT, connectTimeout = CONNECT_TIMEOUT, verifySslCerts = false)
      //println(r)
      println(s"fetchUrlAndReturnLinks: Found status code == ${r.statusCode} for url = $url")
      r.statusCode match {
        case 200 =>
          val prefixUrl = url
            .replaceAll("""https://""", "")
            .replaceAll("""http://""", "")
            .split("/")
            .toList
            .headOption
            .getOrElse("")
          val prefixHttp = if (url.contains("https://")) {
            "https://"
          } else if (url.contains("http://")) {
            "http://"
          } else ""
          val content = r.text()
          //println(s"Google response: $content")
          val doc: Document = Jsoup.parse(content)
          val origlinks: Seq[(String, String)] = {
            doc
              .select("a")
              .asScala
              .map { elm =>
                (elm.attr("href").trim, elm.text())
              }
              .toSeq
          }
          origlinks
        case _ =>
          Seq.empty
      }
    }
  }

    def parse(
               url: String,
               cacheFolder: String,
               isDownload: Boolean,
               counter: Int,
               negativeTitleKeywords: List[String] = List.empty,
               negativeUrlKeywords: List[String] = List.empty,
               positiveTitleKeywords: List[String] = List.empty,
               positiveUrlKeywords: List[String] = List.empty,
    depth: Int = 0,
               isReParsing: Boolean = false
  ): (String, Option[SiteCrawlerResult]) = {
    //val r: zio.ZIO[zio.blocking.Blocking, Throwable, Option[SiteCrawlerResult]] = zio.blocking.effectBlocking {
      try {
        val originalInputUrl = url
        val params = Map()
        println(s"Calling $url with params = $params and counter = $counter")
        val isStopRunning = false
        //isNegativeUrlFound(url, negativeUrlKeywords)
        isStopRunning match {
          case true =>
            val msg = s"Found negative keywords while calling $url with params = $params and counter = $counter, so result = None"
            (msg, None)
          case _ =>
            println(s"Running url via requests now = $url")
            if (isUrlAlive(url)) {
              val linkExt = url.split("""\.""").reverse.head.toLowerCase
              val isPDF = linkExt.equalsIgnoreCase("pdf")
              //val r = requests.get(url, params = params, verifySslCerts = false)
              val (isOk: Boolean, htmlOrFileData: String) = {
                if (isPDF) {
                  (true, "")
                } else {
                  println(s"Running alive url for pdf links via requests now = $url and counter = $counter")
                  //if (counter <= 0) {
                  val r = requests.get(url, headers = Map("user-agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_1) AppleWebKit/537.36 (K HTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36"), params = params, readTimeout = READ_TIMEOUT, connectTimeout = CONNECT_TIMEOUT, verifySslCerts = false, maxRedirects = 10)
                  //println(r)
                  //println(s"Found status code == ${r.statusCode} for url = $url")
                  val ok = r.statusCode == 200
                  (ok, if (ok) {
                    val content = r.text()
                    content
                   } else "")
                  /*
                  } else {
                      val tgt = app.utils.Utils.getSimpleUID
                      val (content: String, finalUrl: String) = downloadAndGetContent(url = url, targetName = tgt, counter = 0, debug = true)
                      if (content.isEmpty()) (false, "") else (true, content)
                  }
                  */
                }
              }
              isOk match {
                case true =>
                  val prefixUrl = url
                    .replaceAll("""https://""", "")
                    .replaceAll("""http://""", "")
                    .split("/")
                    .toList
                    .headOption
                    .getOrElse("")
                  val prefixHttp = if (url.toLowerCase.contains("https://")) {
                    "https://"
                  } else if (url.toLowerCase.contains("http://")) {
                    "http://"
                  } else ""
                  val content = htmlOrFileData
                  //println(s"File response: $content")
                  val origlinks: Seq[(String, String)] = if (isPDF) {
                    Seq((url, ""))
                  } else
                  {
                    val doc: Document = Jsoup.parse(content)
                    doc
                      .select("a")
                      .asScala
                      .map { elm =>
                        (elm.attr("href").trim, elm.text())
                      }
                      .toSeq
                  }
                  val totalCount = origlinks.size
                  val tokenUrls: Seq[(String, String)] = makeCompleteUrls(prefixUrl, prefixHttp, origlinks)
                  println(s"Original links: $origlinks\n\ntoken urls = $tokenUrls for url = $url")
                  val positiveUrlWithLabels: Seq[(String, String)] = tokenUrls
                    .filter { link => {
                      val out = isPositiveKeywordFound(link._2, positiveTitleKeywords)
                      val isSelf = originalInputUrl.equalsIgnoreCase(link._1)
                      !out.isEmpty || isSelf
                    }
                    }
                  val urlMatchedMap: scala.collection.mutable.Map[String, (String, String)] = scala.collection.mutable.Map.empty
                  val links: Seq[(String, String)] = tokenUrls
                    .filter(x => x._1.endsWith(".pdf") || x._1.endsWith(".PDF") || x._1.endsWith(".Pdf"))
                    .filter { link => {
                      val surl = link._1.replaceAll(prefixUrl, "").split("/").tail.mkString("/")
                      val a = isNegativeUrlFound(surl, negativeUrlKeywords)
                      val b = isNegativeKeywordFound(link._2, negativeTitleKeywords)
                      val e = isPositiveUrlFound(surl, positiveUrlKeywords)
                      val f = isPositiveKeywordFound(link._2, positiveTitleKeywords)
                      val isValid1 = !a.isEmpty || !b.isEmpty
                      val isValid2 = !e.isEmpty || !f.isEmpty
                      val leftright:(String, String) = if (isValid1 && isValid2) {
                        (s"$a,$b", s"$e,$f")
                      } else if (isValid1 && !isValid2) {
                        (s"$a,$b", "")
                      } else if (!isValid1 && isValid2) {
                        ("", s"$e,$f")
                      } else ("", "")
                      urlMatchedMap.put(link._1, leftright)
                      val out = (isValid1 == false) || isValid2
                      println(s"Link = $surl  1 = $isValid1 and 2 = $isValid2 with left right = $leftright and final out = $out\n")
                      out
                    }
                    }
                  val noItems = links.size
                  //println(s"Running $url with links = $links has totalCount = $totalCount and filtered no items = $noItems and links = $links")
                  val isRunInDepth = (positiveUrlWithLabels.size > noItems) || (noItems <= 0)
                  println(s"Running $url with links totalCount = $totalCount and filtered count = $noItems with NO of positive keyword urls = ${positiveUrlWithLabels.size} and isRunInDepth = $isRunInDepth with depth = $depth")
                  val finalLinks: Seq[(String, String)] = {
                    val tmplinks = if (isRunInDepth){
                      positiveUrlWithLabels.map(purl => makeCompleteUrls(prefixUrl, prefixHttp, fetchUrlAndReturnLinks(purl._1))).flatten.toList
                    } else tokenUrls
                    //println(s"tmp links = $tmplinks")
                    tmplinks
                      .filter(x => x._1.endsWith(".pdf") || x._1.endsWith(".PDF") || x._1.endsWith(".Pdf"))
                      .filter { link => {
                        val surl = link._1.replaceAll(prefixUrl, "").split("/").tail.mkString("/")
                        val a = isNegativeUrlFound(surl, negativeUrlKeywords)
                        val b = isNegativeKeywordFound(link._2, negativeTitleKeywords)
                        val e = isPositiveUrlFound(surl, positiveUrlKeywords)
                        val f = isPositiveKeywordFound(link._2, positiveTitleKeywords)
                        val isValid1 = !a.isEmpty || !b.isEmpty
                        val isValid2 = !e.isEmpty || !f.isEmpty
                        val leftright:(String, String) = if (isValid1 && isValid2) {
                          (s"$a,$b", s"$e,$f")
                        } else if (isValid1 && !isValid2) {
                          (s"$a,$b", "")
                        } else if (!isValid1 && isValid2) {
                          ("", s"$e,$f")
                        } else ("", "")
                        urlMatchedMap.put(link._1, leftright)
                        val out = (isValid1 == false) || isValid2
                        //println(s"Link = $link  1 = $isValid1 and 2 = $isValid2 and final out = $out\n")
                        out
                      }
                      }
                  }
                  val names = {
                    finalLinks.map(x =>
                      (
                        x._1.replaceAll("""[^0-9a-zA-Z]+""", "-").toLowerCase.replaceAll("""-pdf""", "") + ".pdf",
                        x._1.toLowerCase
                          .replaceAll("""https://""", "")
                          .replaceAll("""http://""", "")
                          .split("/")
                          .toList
                          .headOption
                          .getOrElse(""),
                        x._1.toLowerCase
                          .replaceAll("""https://""", "")
                          .replaceAll("""http://""", "")
                          .split("/")
                          .toList
                          .tail
                          .mkString("-")
                      )
                    )
                  }
                  val paths: Seq[(String, String)] = {
                    finalLinks.zipWithIndex.map { x =>
                      val pathprefix = names(x._2)._2
                      val targetName = s"""${cacheFolder}/${pathprefix}_${names(x._2)._1}"""
                      val (newTarget: String, newUrl: String) = if (isDownload) downloadAndCacheUrlToFile(url = x._1._1, targetName = targetName, forceDownload = isReParsing) else (targetName, url)
                      if (newTarget.isEmpty()) ("", "")
                      else {
                        if (isDownload) {
                          try {
                            val istargetExists = new File(newTarget).exists()
                            if (istargetExists) {
                            val name = names(x._2)._3
                            val ext = name.split("""\.""").reverse.head
                            val s3filename = "pdf/" + names(x._2)._2 + "/" + shared.utils.Slug.slugify(name) + s".$ext"
                            app.utils.Utils.uploadToS3(
                              newTarget,
                              s3filename
                            ) match {
                              case Right(value) =>
                                val file = new File(newTarget)
                                if (file.exists()) {
                                  //file.delete()
                                }
                                (newTarget, value)
                                //(s3filename, value)
                              case Left(e) =>
                                println(e)
                                (newTarget, "")
                            }

                            } else {
                                println(s"New target found = $newTarget does not exists for url = $url, so, AWS upload stopped.")
                                (newTarget, "")
                            }
                          } catch {
                            case e: Exception =>
                              //e.printStackTrace()
                              println(s"S3 upload for target file = $targetName / new target = $newTarget has error")
                              (newTarget, "")
                          }
                        } else {
                          (newTarget, "")
                        }
                      }
                    }
                  }
                  val leftright: (String, String) = urlMatchedMap.get(url).getOrElse(("", ""))
                  ("",
                  Some(
                    SiteCrawlerResult(
                      sourceUrl = url,
                      links = finalLinks.map(_._1),
                      labels = finalLinks.map(_._2),
                      names = names.map(_._1),
                      paths = paths.map(_._1),
                      s3paths = paths.map(_._2),
                      totalCount = Some(totalCount),
                      validUrlCount = Some(noItems),
                      negativeMatches = Some(leftright._1),
                      positiveMatches = Some(leftright._2),
                      //blob = Some(content)
                      blob = None
                    )
                  )
                  )
                case _ =>
                  val msg = s"Site url, $url has non valid status code = ${isOk}"
                  (msg, None.asInstanceOf[Option[SiteCrawlerResult]])

              }
            } else {
                  val msg = s"URL = $url seems to be dead."
              (msg, None.asInstanceOf[Option[SiteCrawlerResult]])
              }
            }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          val error = app.utils.Utils.stackTrace(e)
          println(s"ERROR = $error for url = $url")
          app.utils.Utils.writeLog(s"${cacheFolder}/url_errors.txt", url,  appendNewline = true)
          if (counter <= 0) {
            if (error.contains("UnknownHostException")) {
              ("UnknownHostException", None.asInstanceOf[Option[SiteCrawlerResult]])
            } else {
              val newurl: String = if (url.startsWith("https:")) {
                  url.replaceAll("https:", "http:")
              } else if (url.startsWith("http:")) {
                url.replaceAll("http:", "https:")
              } else url
              parse(newurl, cacheFolder, isDownload, counter + 1, negativeTitleKeywords, negativeUrlKeywords, positiveTitleKeywords, positiveUrlKeywords)
            }
          } else (error, None.asInstanceOf[Option[SiteCrawlerResult]])
      }
    //}
    //implicit val runtime = zio.Runtime.default
    //runtime.unsafeRun(r)
  }
  
  def downloadAndGetContent(url: String, targetName: String, counter: Int = 0, debug: Boolean = false): (String, String) = {
    try {
        if (debug)
          println(s"downloadAndGetContent: Target file at $targetName for url = $url download starting")
        if (isUrlAlive(url)) {
          val cmd = "wget"
          //10 seconds --dns-timeout=, --connect-timeout, and --read-timeout=
          val args = if (url.toLowerCase.startsWith("https"))
            Seq(url,"--load-cookies=cookies.txt", "--no-check-certificate","--max-redirect", "--no-verbose", "--waitretry=0", "--retry-connrefused", "--quiet", "--timeout=10",  "--tries=2", "--user-agent=\"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36\"", "-O", targetName)
          else
            Seq(url,"--load-cookies=cookies.txt", "--max-redirect", "--no-verbose", "--waitretry=0", "--retry-connrefused", "--quiet", "--timeout=10",  "--tries=2", "--user-agent=\"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36\"", "-O", targetName)
          val output: Either[String, String] = utils.ShellUtils.executeViaProcessBuilder(cmd, args, debug = debug)
          output match {
            case Right(value) =>
              //println(s"Url = $url with target name = $targetName has result = $value")
              println(s"downloadAndGetContent: Url = $url with target name = $targetName via wget")
              (value, url)
            case Left(error) =>
              println(s"downloadAndGetContent: ERROR: Download for url = $url and file path = $targetName as $error")
              ("", url)
          }
        } else {
          if (debug)
            println(s"downloadAndGetContent: Target file at $targetName for url = $url download stopped because url is inactive")
          ("", url)
        }
    } catch {
      case e: Throwable =>
        val error = app.utils.Utils.stackTrace(e)
        if (debug)
          println(s"downloadAndGetContent: Error in downloading file via wget at $targetName with error = $error from url: $url")
        else
          println(s"downloadAndGetContent: Error in downloading file via wget at $targetName from url: $url")
        if (counter >= 1) {
          var finalUrl = url
          Try {
            val status = utils.ShellUtils.fileDownloader(url, targetName, CONNECT_TIMEOUT, READ_TIMEOUT, allowRedirect = false)
            if (debug)
              println(s"downloadAndGetContent: Status = $status for url download = $url")
            val lines = scala.io.Source.fromFile(targetName).getLines().toList
            val data = lines.mkString(",")
            if (data.contains("301 Moved Permanently") && data.contains("Location:")) {
              lines.foreach(line => {
                if (line.contains("Location:")) {
                  val tokens = line.split("Location:").toList
                  val newUrl = tokens(1).trim
                  println(s"downloadAndGetContent: NEW URL = $newUrl")
                  finalUrl = newUrl
                }
              })
            }
          }
          if (finalUrl.equalsIgnoreCase(url)) {
            val content = scala.io.Source.fromFile(targetName).getLines().mkString
            (content, finalUrl)
          } else {
            val ext = finalUrl.toLowerCase.split("""\.""").reverse.head
            val finalTarget = s"${targetName}_${counter}.$ext"
            val status = utils.ShellUtils.fileDownloader(finalUrl, finalTarget, CONNECT_TIMEOUT, READ_TIMEOUT, allowRedirect = true)
            val content = scala.io.Source.fromFile(finalTarget).getLines().mkString
            (content, finalUrl)
          }
        } else {
          if (counter <= 0) {
            downloadAndGetContent(ShellUtils.changeHttpHttps(url), targetName, counter + 1, debug)
          } else {
            println(s"downloadAndGetContent: Error in downloading file via wget at $targetName from url: $url with counter = $counter == $error")
            ("", url)
          }
        }
    }
  }


  def asJson(obj: SiteCrawlerResult): String = {
      import io.circe.Printer
      import io.circe.generic.auto._, io.circe.syntax._
      val printer = Printer.noSpaces.copy(dropNullValues = true)
      obj.asJson.printWith(printer)
    }
}
