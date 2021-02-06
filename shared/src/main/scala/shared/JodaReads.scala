package shared
import java.time.{Instant, LocalDate, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import play.api.libs.json._

object JavaReads extends JavaReads

trait JavaReads {
  def getZoneId(zone: String) = ZoneId.of(zone)
  def zonedDateTimeReads(pattern: String, zone: String = "UTC", corrector: String => String = identity): Reads[ZonedDateTime] = new Reads[ZonedDateTime] {

    val df = if (pattern == "") DateTimeFormatter.ISO_ZONED_DATE_TIME else DateTimeFormatter.ofPattern(pattern)

    def reads(json: JsValue): JsResult[ZonedDateTime] = json match {
      case JsNumber(d) => JsSuccess(ZonedDateTime.ofInstant(Instant.ofEpochMilli(d.longValue), getZoneId(zone)))
      case JsString(s) => parseDate(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date.format", pattern))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
    }

    private def parseDate(input: String): Option[ZonedDateTime] =
      scala.util.control.Exception.nonFatalCatch[ZonedDateTime] opt (ZonedDateTime.parse(input, df))

  }

  def localDateReads(pattern: String, zone: String = "UTC", corrector: String => String = identity): Reads[LocalDate] = new Reads[LocalDate] {

    val df = if (pattern == "") DateTimeFormatter.ISO_LOCAL_DATE else DateTimeFormatter.ofPattern(pattern)

    def reads(json: JsValue): JsResult[LocalDate] = json match {
      case JsNumber(d) => {
        val dt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(d.longValue), getZoneId(zone))
        JsSuccess(LocalDate.of(dt.getYear, dt.getMonth, dt.getDayOfMonth))
      }
      case JsString(s) => parseDate(corrector(s)) match {
        case Some(d) => JsSuccess(d)
        case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date.format", pattern))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError("error.expected.date"))))
    }

    private def parseDate(input: String): Option[LocalDate] =
      scala.util.control.Exception.nonFatalCatch[LocalDate] opt (LocalDate.parse(input, df))

  }
  implicit val defaultJavaZonedDateTimeReads = zonedDateTimeReads("")
  implicit val defaultJavaLocalDateTimeReads = localDateReads("")
}
