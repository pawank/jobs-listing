package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._


case class GoogleSearchResult(q: String, notificationLink: String, notificationPdf: String, value: Seq[String], names: Seq[String], paths: Seq[String], params: String, blob: Option[String], ok: Boolean, error: String = "") {
  def isOk(ignoreSites: Seq[String]): Boolean = {
    value.map(url => {
      if (ignoreSites.isEmpty) true else {
        ignoreSites.foldLeft(false)((a, b) => a || url.toLowerCase.contains(b.toLowerCase))
      }
    }).filter(b => b == true).headOption.getOrElse(false)
  }
}

object GoogleSearchResult {
  implicit val jsonFormatNameValueGoogleSearchResult: Format[GoogleSearchResult] = Json.format[GoogleSearchResult]
}


