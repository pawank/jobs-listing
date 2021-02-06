package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class SourceMetadata(path: String, hash: Option[String] = None, 
                          url: Option[String] = None,
                          sourceUrl: Option[String] = None,
                          title: Option[String] = None,
                          name: Option[String] = None,
                          downloadPath: Option[String] = None,
                          s3Path: Option[String] = None,
                          publicUrl: Option[String] = None,
                          negativeMatches: Option[String] = None,
                          positiveMatches: Option[String] = None
                         )

object SourceMetadata {
  implicit val jsonFormatSourceMetadata: Format[SourceMetadata] = Json.format[SourceMetadata]

}

