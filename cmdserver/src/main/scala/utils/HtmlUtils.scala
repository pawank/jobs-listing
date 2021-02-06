package server.utils

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.libs.json.{JsArray, JsValue}

object HtmlUtils {

  def getContentFromUrl(url: String): Option[String] = {
    val r = requests.get(url, readTimeout = 120, connectTimeout = 1200)
    r.statusCode match {
      case 200 =>
        val content = r.text()
        Some(content)
      case _ =>
        None
    }
  }

  def getContentByHtmlId(id: String, html: String): Option[String] = {
    val doc: Document = Jsoup.parse(html)
    val foundshotData = html.contains("shotData")
    println(foundshotData)
    None
  }

  def getItemData(html: String) = {
    import io.circe.generic.auto._
    import io.circe.parser.decode
    import io.circe.syntax._
    import cats.syntax.either._
    import io.circe._
    import io.circe.parser._
    import io.circe.optics.JsonPath._
    val j: Json = Json.fromString(html)
    println(s"JSON: $j")
    val data1 = root.Styles.each.ToneOfVoice.string.getAll(j)
    val data2 = root.Styles.each.Fits.each.Items.each.WashingInstructions.string.getAll(j)
    import play.api.libs.json.{JsArray, JsObject}
    val json = play.api.libs.json.Json.parse(html)
    val data3 = (json \\ "Styles").map(s => (s \ "ToneOfVoice").asOpt[String])
    val data4 = (json \\ "Styles").size
    //val data4 = (json \\ "Styles").map(s => (s \\ "Fits").map(x => (x \\ "Items")))
    val result = s"data1: $data1 and data2: $data2"
    println(s"3: $data3 and 4: $data4")
    result
  }

  def getContentFromUrl(url: String, id: String): Option[String] = {
    if (true) {
      val parser = new services.ParserSelenium()
      parser.setUp()
      val data = parser.executeUrlAndExtractInfo(url, "").replaceAll("""text - Show Less""","").split("""To view our""").headOption
      data
    } else {

      val html = getContentFromUrl(url).getOrElse("")
      val doc: Document = Jsoup.parse(html)
      val foundshotData = html.contains("shotData")
      if (foundshotData) {
        val dataJson = html.split("""var shotData = """).tail.mkString("").split("""</script>""").headOption.getOrElse("").trim.reverse.tail.reverse
        //println(dataJson)
        //app.utils.Utils.saveToFile("/tmp/1.json", dataJson, prefix = "", appendNewline = false)
        Some(getItemData(dataJson))
        //Some(dataJson)
      } else None
    }
  }

  def getContentFromUrls(urls: List[String], id: String): List[(String, String)] = {
    val dataoutput = "products.txt"
    val parser = new services.ParserSelenium()
    parser.setUp()
    val xs = urls.map(url => {
        println(s"Processing url, $url")
        val data = parser.executeUrlAndExtractInfo(url, "").replaceAll("""text - Show Less""","").split("""To view our""").headOption.getOrElse("")
        val txt = s"$url^${data}"
        app.utils.Utils.writeLog(dataoutput, txt, appendNewline = true)
        (url, data)
      })
            try {
              parser.tearDown()
            } catch {
              case ex: Exception =>
                ex.printStackTrace()
            }
    xs
  }


}
