package api

import java.io.File
import java.net.InetSocketAddress
import java.time.{LocalDate, ZoneId}

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import os.Path
import shared.{DatesInfo, JobExtra, SimpleJob, SiteCrawler, SiteCrawlerResult, SiteUrl}

import scala.jdk.CollectionConverters._
import scala.util.Try

object PartnerCrawler {
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

  val APP_BASE_HTTP_URL = {
    val url = System.getenv("APP_BASE_HTTP_URL")
    if (url == null) "https://www.sarkarijoblisting.com" else url
    url.replaceAll("""https://""","").replaceAll("""http://""","").trim
  }

  val TEXT_LIMIT = 1000
  val NO_OF_PAGES_FOR_MATCH = 2

  import java.io.IOException
  val convertedUrlToOriginalUrlMap:scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty
  val originalUrlToConvertedMap:scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map.empty

  def siteCrawlerToEnhancedSimpleJon(originalPdf: String, crawler: Option[SiteCrawler]) = {
    val simpleJobs = (for {
      c <- crawler
      r <- c.result
      cr <- r.simpleJobs
    } yield cr).getOrElse(Seq.empty)
    val matchedSimple = simpleJobs.filter(s => s.originalPdf.equalsIgnoreCase(originalPdf)).headOption
    val startdt: Option[LocalDate] = matchedSimple.map(m => {
      app.utils.Utils.parseDateFromText(m.postDate).headOption.map(dt => dt.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
    }).flatten
    val enddt: Option[LocalDate] = matchedSimple.map(m => {
      app.utils.Utils.parseDateFromText(m.lastDate).headOption.map(dt => dt.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
    }).flatten
    matchedSimple.map(s => s.copy(startDate = startdt, endDate = enddt))
  }

  def getAllLinks(tds: List[Element], originalDomain: String, isReplaceDomain: Boolean): Seq[(String, String)] = {
    val links: Seq[(String, String)] = tds(1).select("a")
      .asScala
      .map { elm => {
        (elm.attr("href").trim, elm.text())
        }
      }.toSeq
    if (isReplaceDomain) {
      links.map(link => {
        //val domain = getJustDomain(link._1)
        if (link._1.contains(originalDomain)) {
          val converted = link._1.replaceAll(originalDomain,APP_BASE_HTTP_URL)
          convertedUrlToOriginalUrlMap.put(converted, link._1)
          originalUrlToConvertedMap.put(link._1, converted)
          (converted, link._2)
        } else {
          link
        }
      })
    } else links
  }

  def getLinksFromTable(content: String, originalDomain: String): Seq[(String, String, String)] = {
    val jobs: scala.collection.mutable.ListBuffer[(String, String, String)] = scala.collection.mutable.ListBuffer.empty
    val doc: Document = Jsoup.parse(content)
    doc.select("table").iterator().asScala.toList.map(table => {
      var notificationIdx = -1
      var officalSiteIdx = -1
      var notification = ""
      var website = ""
      table.select("tr").iterator().asScala.toList.map(row => {
        table.select("tr").iterator().asScala.toList.map(row => {
          val tds = row.select("td").iterator().asScala.toList
          val sz = tds.size
          var part1: String = if (sz >= 2) tds(0).text().trim else ""
            part1.toLowerCase match {
              case "notification" =>
                val data = tds(1).text().trim
                val links = getAllLinks(tds, originalDomain, isReplaceDomain = true)
                links.map(s => {
                  jobs.addOne((s._1, s._2, "notification"))
                })
                //println(s"Part1 $part1 and links = $links")
                /*
              case "registration" =>
                val data = tds(1).text().trim
                val links = getAllLinks(tds)
                //println(s"Part1 $part1 and links = $links")
                links.map(s => {
                  jobs.addOne((s._1, s._2, "apply online"))
                })
              case "apply online" =>
                val data = tds(1).text().trim
                val links = getAllLinks(tds)
                //println(s"Part1 $part1 and links = $links")
                links.map(s => {
                  jobs.addOne((s._1, s._2, "apply online"))
                })*/
              case "official website" =>
                val data = tds(1).text().trim
                val links = getAllLinks(tds, originalDomain, isReplaceDomain = true)
                //println(s"Part1 $part1 and links = $links")
                links.map(s => {
                  jobs.addOne((s._1, s._2, "website"))
                })
              case _ =>
            }
        })
      })
    })
    jobs.toSeq
  }

  def getJustDomain(url: String): String = {
      url
      .replaceAll("""https://""", "")
      .replaceAll("""http://""", "")
      .split("/")
      .toList
      .headOption
      .getOrElse("").replaceAll("""www.""","")
  }

  def tableToJson(doc: Document, originalDomain: String) = {
    val jobs: scala.collection.mutable.ListBuffer[SimpleJob] = scala.collection.mutable.ListBuffer.empty
    doc.select("table").iterator().asScala.toList.map(table => {
      var postDate = -1
      var board = -1
      var postName = -1
      var qualification = -1
      var advNo = -1
      var lastDate = -1
      var more = -1
      var walkIn = ""
      var originalPdf = ""
      table.select("tr").iterator().asScala.toList.map(row => {
          val ths = row.select("th").iterator().asScala
          ths.zipWithIndex.foreach(thIdx => {
            val th = thIdx._1
            th.text().trim match {
              case "Post Date" =>
                postDate = thIdx._2
              case "Recruitment Board" =>
                board = thIdx._2
              case "Post Name" => postName = thIdx._2
              case "Qualification" =>
                qualification = thIdx._2
              case "Advt No" => advNo = thIdx._2
              case "Last Date" =>
                lastDate = thIdx._2
              case "More Information" =>
                more = thIdx._2
              case _ =>
            }
          })
        var ext = SimpleJob(board = "", originalPdf = "", postDate = "", lastDate = "", qualification = "", advNo = "", title = "", url = "", walkIn = "")
          val tds = row.select("td").iterator().asScala
        tds.zipWithIndex.foreach(tdidx => {

            val td = tdidx._1
            val data = td.text().trim
            if (postDate == tdidx._2) {
              ext = ext.copy(postDate = data)
            }
            if (board == tdidx._2) {
              ext = ext.copy(board = data)

            }
             if (postName == tdidx._2) {
               ext = ext.copy(title = data)

             }
            if (qualification == tdidx._2) {

              ext = ext.copy(qualification = data)
            }
            if (advNo == tdidx._2) {

              ext = ext.copy(advNo = data)
            }
            if (lastDate == tdidx._2) {
              val date = if (data.contains(" ")) {
                if (data.contains("Walk")) {
                  walkIn = "Walk-in"
                  data.split(" ").toList.head
                } else {
                  data.split(" ").toList.reverse.head
                }
              } else data
              ext = ext.copy(lastDate = date)
              ext = ext.copy(walkIn = walkIn)
            }
            if (more == tdidx._2) {
              if (data.contains("Get Details..")) {
                val links: Seq[(String, String)] = td.select("a")
                  .asScala
                  .map { elm =>
                    (elm.attr("href").trim, elm.text())
                  }.toSeq
                originalPdf = links.head._1
                ext = ext.copy(originalPdf = originalPdf, originalLabel = "Get Details..")
              }
              else if (data.contains("Get Details")) {
                val links: Seq[(String, String)] = td.select("a")
                  .asScala
                  .map { elm =>
                    (elm.attr("href").trim, elm.text())
                  }.toSeq
                val pageUrl = links.head._1
                val allLinks = SiteParserAndCrawler.fetchUrlAndReturnLinks(pageUrl).distinct
                val content = SiteParserAndCrawler.fetchHtmlContent(pageUrl)
                val notificationAndSiteLinksWithtype = getLinksFromTable(content, originalDomain).distinct
                val notificationAndSiteLinks = notificationAndSiteLinksWithtype.map(s => (s._1, s._2))
                //println(s"Notification and site links = $notificationAndSiteLinks")
                val prefixUrl = getJustDomain(pageUrl)
                val opts = allLinks.filter(x => {
                  x._2.toLowerCase.contains("click here") && (!x._1.contains(prefixUrl)) && (x._1.toLowerCase.startsWith("http"))
                })
                //println(s"All links from actual page = ${pageUrl} == $opts")
                val opt = opts.reverse.headOption
                originalPdf = opt.map(_._1).getOrElse("")
                val originalLabel = opt.map(_._2).getOrElse("")
                if (!originalPdf.isEmpty) {
                  ext = ext.copy(originalPdf = originalPdf, originalLabel = originalLabel)
                  //println(s"Original pdf on actual Ad page = $originalPdf")
                }
                notificationAndSiteLinks.map(s => {
                  val newExt = ext.copy(originalPdf = s._1, url = s._2, links = Some(notificationAndSiteLinksWithtype.map(n => SiteUrl(n._1, n._2, n._3))))
                  //println(s"Simple Job with notification link = $newExt")
                  jobs.addOne(newExt)
                })
              }
            }
          })
        //println(s"Simple Job = $ext")
        jobs.addOne(ext)

      })
    })
    jobs.toList
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
      val originalDomain = getJustDomain(url)
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
            if (SiteParserAndCrawler.isUrlAlive(url)) {
              val linkExt = url.split("""\.""").reverse.head.toLowerCase
              val isPDF = linkExt.equalsIgnoreCase("pdf")
              //val r = requests.get(url, params = params, verifySslCerts = false)
              val (isOk: Boolean, htmlOrFileData: String) = {
                if (isPDF) {
                  (true, "")
                } else {
                  val r = requests.get(url, params = params, readTimeout = READ_TIMEOUT, connectTimeout = CONNECT_TIMEOUT, verifySslCerts = false)
                  //println(r)
                  //println(s"Found status code == ${r.statusCode} for url = $url")
                  val ok = r.statusCode == 200
                  (ok, if (ok) r.text() else "")
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
                  //println(s"Google response: $content")
                  val simpleJobs: List[SimpleJob] = {
                    val doc: Document = Jsoup.parse(content)
                    tableToJson(doc, originalDomain)
                  }
                  val linkToSimpleJob: Map[String, SimpleJob] = simpleJobs.map(j => {
                    (j.originalPdf, j)
                  }).toMap
                  val origlinks: Seq[(String, String)] = simpleJobs.map(j => {
                    (j.originalPdf, j.originalLabel)
                  })
                  /*
                  val origlinks: Seq[(String, String)] = if (isPDF) {
                    Seq((url, ""))
                  } else
                  {
                    val doc: Document = Jsoup.parse(content)
                    val allLinks = doc
                      .select("a")
                      .asScala
                      .map { elm =>
                        (elm.attr("href").trim, elm.text())
                      }
                      .toSeq
                    allLinks
                  }*/
                  val totalCount = origlinks.size
                  //println(s"Original links: $origlinks")
                  val tokenUrls: Seq[(String, String)] = SiteParserAndCrawler.makeCompleteUrls(prefixUrl, prefixHttp, origlinks)
                  val positiveUrlWithLabels: Seq[(String, String)] = tokenUrls
                    .filter { link => {
                      val out = SiteParserAndCrawler.isPositiveKeywordFound(link._2, positiveTitleKeywords)
                      val isSelf = originalInputUrl.equalsIgnoreCase(link._1)
                      !out.isEmpty || isSelf
                    }
                    }
                  val urlMatchedMap: scala.collection.mutable.Map[String, (String, String)] = scala.collection.mutable.Map.empty
                  val links: Seq[(String, String)] = tokenUrls
                    .filter(x => x._1.endsWith(".pdf") || x._1.endsWith(".PDF") || x._1.endsWith(".Pdf"))
                    .filter { link => {
                      val surl = link._1.replaceAll(prefixUrl, "").split("/").tail.mkString("/")
                      val a = SiteParserAndCrawler.isNegativeUrlFound(surl, negativeUrlKeywords)
                      val b = SiteParserAndCrawler.isNegativeKeywordFound(link._2, negativeTitleKeywords)
                      val e = SiteParserAndCrawler.isPositiveUrlFound(surl, positiveUrlKeywords)
                      val f = SiteParserAndCrawler.isPositiveKeywordFound(link._2, positiveTitleKeywords)
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
                      //println(s"Link = $surl  1 = $isValid1 and 2 = $isValid2 with left right = $leftright and final out = $out\n")
                      out
                    }
                    }
                  val noItems = links.size
                  //println(s"Running $url with links = $links has totalCount = $totalCount and filtered no items = $noItems and links = $links")
                  val isRunInDepth = (positiveUrlWithLabels.size > noItems) || (noItems <= 0)
                  println(s"Running $url with links totalCount = $totalCount and filtered count = $noItems with NO of positive keyword urls = ${positiveUrlWithLabels.size} and isRunInDepth = $isRunInDepth with depth = $depth")
                  val finalLinks: Seq[(String, String)] = {
                    val tmplinks = if (isRunInDepth){
                      positiveUrlWithLabels.map(purl => SiteParserAndCrawler.makeCompleteUrls(prefixUrl, prefixHttp, SiteParserAndCrawler.fetchUrlAndReturnLinks(purl._1))).flatten.toList
                    } else tokenUrls
                    //println(s"tmp links = $tmplinks")
                    tmplinks
                      .filter(x => x._1.endsWith(".pdf") || x._1.endsWith(".PDF") || x._1.endsWith(".Pdf"))
                      .filter { link => {
                        val surl = link._1.replaceAll(prefixUrl, "").split("/").tail.mkString("/")
                        val a = SiteParserAndCrawler.isNegativeUrlFound(surl, negativeUrlKeywords)
                        val b = SiteParserAndCrawler.isNegativeKeywordFound(link._2, negativeTitleKeywords)
                        val e = SiteParserAndCrawler.isPositiveUrlFound(surl, positiveUrlKeywords)
                        val f = SiteParserAndCrawler.isPositiveKeywordFound(link._2, positiveTitleKeywords)
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
                      val (newTarget: String, newUrl: String) = {
                        val urlToDowload = convertedUrlToOriginalUrlMap.get(x._1._1) match {
                          case Some(found) => found
                          case _ => x._1._1
                        }
                        println(s"\n\nConverted url = ${x._1._1} and orig = ${urlToDowload} with isDownload = $isDownload")
                        val (tgt: String, fnlUrl: String) = if (isDownload) SiteParserAndCrawler.downloadAndCacheUrlToFile(url = urlToDowload, targetName = targetName, debug = true, forceDownload = isReParsing) else (targetName, url)
                        val converted = originalUrlToConvertedMap.get(fnlUrl).getOrElse(fnlUrl)
                        println(s"\n\nOrig url = ${fnlUrl} and converted url = ${converted} and new target = $tgt")
                        (tgt, converted)
                      }
                      if (newTarget.isEmpty()) ("", "")
                      else {
                        if (isDownload) {
                          try {
                            val istargetExists = new File(newTarget).exists()
                            if (istargetExists) {
                            val name = names(x._2)._3.replaceAll(originalDomain,APP_BASE_HTTP_URL)
                            val ext = name.split("""\.""").reverse.head
                            val s3filename = "pdf/" + names(x._2)._2.replaceAll(originalDomain,APP_BASE_HTTP_URL) + "/" + shared.utils.Slug.slugify(name) + s".$ext"
                            println(s"Target exists = $istargetExists and s3filename = $s3filename before S3 upload with new target = $newTarget")
                            app.utils.Utils.uploadToS3(
                              newTarget,
                              s3filename
                            ) match {
                              case Right(value) =>
                                val file = new File(newTarget)
                                if (file.exists()) {
                                  //file.delete()
                                }
                                val v = (newTarget, value)
                                println(s"AWS tuple for url = $url == ${v}")
                                v
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
                      links = finalLinks.map(_._1).map(_.replaceAll(originalDomain,APP_BASE_HTTP_URL)),
                      labels = finalLinks.map(_._2).map(_.replaceAll(originalDomain,APP_BASE_HTTP_URL)),
                      names = names.map(_._1),
                      paths = paths.map(_._1),
                      s3paths = paths.map(_._2),
                      totalCount = Some(totalCount),
                      validUrlCount = Some(noItems),
                      negativeMatches = Some(leftright._1),
                      positiveMatches = Some(leftright._2),
                      simpleJobs = Some(finalLinks.map(_._1).map(link => {
                        val sj = linkToSimpleJob.get(link).get
                        sj.copy(originalPdf = sj.originalPdf.replaceAll(originalDomain,APP_BASE_HTTP_URL))
                      })),
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
              val msg = "Site seems to be down"
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
    }
}
