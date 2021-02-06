package shared


import java.time.{LocalDate, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import play.api.libs.json.{JsNumber, JsString, JsValue, Writes}

object JavaWrites extends JavaWrites

trait JavaWrites {
  def getZoneId(zone: String) = ZoneId.of(zone)

  def localDateWrites(pattern: String): Writes[ZonedDateTime] = new Writes[ZonedDateTime] {
    val df = DateTimeFormatter.ofPattern(pattern)
    def writes(d: ZonedDateTime): JsValue = JsString(d.format(df))
  }

  def zonedDateTimeWrites(pattern: String): Writes[ZonedDateTime] = new Writes[ZonedDateTime] {
    val df = DateTimeFormatter.ofPattern(pattern)
    def writes(d: ZonedDateTime): JsValue = JsString(d.format(df))
  }

  //implicit object ZonedDateTimeNumberWrites extends Writes[ZonedDateTime] {
  //  def writes(d: ZonedDateTime): JsValue = JsNumber(d.toInstant.toEpochMilli)
  //}

  implicit object ZonedDateTimeWrites extends Writes[ZonedDateTime] {
    def writes(d: ZonedDateTime): JsValue = JsString(d.toString)
  }

  implicit object LocalDateTimeWrites extends Writes[LocalDate] {
    def writes(d: LocalDate): JsValue = JsString(d.toString)
  }
}