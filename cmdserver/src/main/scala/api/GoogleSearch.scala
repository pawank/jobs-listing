package api

import play.api.libs.json.JsArray
import shared.{GoogleSearchResult}
import utils.extraction.TikaExtraction


object GoogleSearch {
  val TEXT_LIMIT = 1000
  val NO_OF_PAGES_FOR_MATCH = 2

  def performMatchingOfResults(g: GoogleSearchResult, originalContent: String) = {
    val result: Seq[Tuple3[String, String, Boolean]] = g.paths.zipWithIndex.map(x => {
      val targetName = x._1
      TikaExtraction.parseAsHtml(targetName, Some(NO_OF_PAGES_FOR_MATCH)) match {
        case Right(value) =>
          val c1 = originalContent.take(TEXT_LIMIT)
          val c2 = value.content.take(TEXT_LIMIT)
          val matched = if (c1.equalsIgnoreCase(c2)) true else {
            val tokens1 = c1.split(" ")
            val tokens2 = c2.split(" ")
            val diff = tokens1.diff(tokens2)
            if (diff.size < 50) true else false
          }
          (g.value(x._2), if (g.paths.size > x._2) g.paths(x._2) else "", matched)
        case Left(error) =>
          println(s"Error in extraction for file name = ${x._1}")
          (g.value(x._2), if (g.paths.size > x._2) g.paths(x._2) else "", false)
      }
    })
    result.filter(_._3 == true).headOption
  }

  def getGoogleResults(keys: List[String], query: String, cacheFolder: String, isDownload: Boolean = false, counter: Int = 0): List[GoogleSearchResult] = {
    //val q = """Advt No Opening Date "21.09 2019" Closing Date "20.10 2019" OFFICE NOTE Sub Notification for Apprenticeship Training Under Apprenticeship Act 1961 at THDCIL Tehri Uttarakhand THDC India Limited A Mini"""
    //val q = app.utils.Utils.encodeQueryString(query)
    val q = query
    /*
    import io.circe._
    import io.circe.generic.auto._
    import io.circe.parser.{decode, _}
    import io.circe.syntax._

    import io.circe.optics.JsonPath._
    */

    val url = "https://www.googleapis.com/customsearch/v1"
    val results: List[Option[GoogleSearchResult]] = keys.map(key => {
      try {
        //val params = Map("key" -> key, "cx" -> "007332834245290939646:ntlsowsyyx8", "q" -> q)
        val params = Map("key" -> key, "cx" -> "007332834245290939646:ntlsowsyyx8", "as_filetype" -> "pdf", "filetype" -> "pdf", "q" -> q)
        println(s"Calling $url with key = $key and counter = $counter")
        val r = requests.get(url, params = params, readTimeout = 1200, connectTimeout = 1200)
        println(r)
        r.statusCode match {
          case 200 =>
            val content = r.text()
            //println(s"Google response: $content")
            /*
          io.circe.parser.parse(content) match {
            case Right(value) =>
              println(s"Value: $value")
              val x = root.items.arr
            case Left(errors) =>
              println(errors)
          }
           */
            val value = play.api.libs.json.Json.parse(content)
            //println(s"Google response JSON: $value")
            val totalCount = (value \ "searchInformation" \ "totalResults").as[String].toInt
            val noItems = if (totalCount > 0) (value \ "items").as[JsArray].value.size else 0
            val origlinks: Seq[String] = if (totalCount > 0) (value \ "items").as[JsArray].value.map(j => (j \ "link").as[String]).toList.toSeq else Seq.empty
            val links: Seq[String] = origlinks.filter(x => x.endsWith(".pdf") || x.endsWith(".PDF") || x.endsWith(".Pdf"))
            //val links: Seq[String] = Seq.empty
            println(s"Google search for q = $q with original links = $origlinks has totalCount = $totalCount, noItems = $noItems and links = $links")
            import shared.utils.Slug._
            val names = links.map(x => x.replaceAll("""[^0-9a-zA-Z]+""","-").toLowerCase.replaceAll("""-pdf""","") + ".pdf")
            //val names = links.map(x => x.split("/").toSeq.reverse.head.toLowerCase.replaceAll(""".pdf""","").slug)
            val paths = if (isDownload) {
              links.zipWithIndex.map(x => {
                val targetName = s"""${cacheFolder}pdf/${names(x._2)}"""
                try {
                val cmd = "wget"
                val args = Seq(x._1,"--no-check-certificate", "-O", targetName)
                val output: Either[String, String] = utils.ShellUtils.executeViaProcessBuilder(cmd, args)
                output match {
                  case Right(value) =>
                    println(s"Url = $url with target name = $targetName has result = $value")
                    targetName
                  case Left(error) =>
                    println(s"ERROR: Download for url = $x as $error")
                    ""
                }
              } catch {
                case e: Throwable =>
                  e.printStackTrace()
                  val error = app.utils.Utils.stackTrace(e)
                  println(s"Error in downloading google result file at $targetName from url: ${x._1}")
                    ""
              }
              })
            } else Seq.empty
            val isOk = (paths.size == links.size) || (links.size > 0)
            Some(GoogleSearchResult(q = q, notificationLink = "", notificationPdf = "", value = links, names = names, paths = paths, params = params.toString, blob = Some(content), ok = isOk, error = ""))
          case _ =>
            None.asInstanceOf[Option[GoogleSearchResult]]
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          val error = app.utils.Utils.stackTrace(e)
          None.asInstanceOf[Option[GoogleSearchResult]]
      }
    })
    results.filter(_.isDefined).map(_.get)
  }
  def search(q: String): Option[GoogleSearchResult] = {
    val keys = List("AIzaSyBtJSVQNH9UGuHEBU4Y7snSaZCsyO1yxOE", "AIzaSyAz_Coj6WAciHgKM3woLiLdBbkRpH17p9c")
    //val keys = List("AIzaSyBtJSVQNH9UGuHEBU4Y7snSaZCsyO1yxOE")
    val xs1 = getGoogleResults(List(keys.head), q, "", false)
    if (xs1.size > 0) xs1.headOption else {
      getGoogleResults(List(keys.reverse.head), q, "", false).headOption
    }
  }

  def searchAndDownload(keys: List[String], cacheFolder: String, q: String): Option[GoogleSearchResult] = {
    val xs1 = getGoogleResults(List(keys.head), q, cacheFolder = cacheFolder, isDownload = true, counter = 0)
    if (xs1.size > 0) xs1.headOption else {
      val xs2 = getGoogleResults(List(keys.reverse.head), q, cacheFolder = cacheFolder, isDownload = true, counter = 1)
      if (xs2.isEmpty) getGoogleResults(List(keys.head), q, cacheFolder = cacheFolder, isDownload = true, counter = 2).headOption else xs2.headOption
    }
  }
}
