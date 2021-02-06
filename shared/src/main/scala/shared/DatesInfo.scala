package shared

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

/*
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._

import shared.ModelsUtils._
*/
case class DatesInfo(created: ZonedDateTime, by: String, updated: Option[ZonedDateTime] = None, deleted: Option[ZonedDateTime] = None)
object DatesInfo{
  implicit val jsonFormatDatesInfo: Format[DatesInfo] = Json.format[DatesInfo]

  def getCurrentDatesInUTC(): DatesInfo = {
    DatesInfo(created = ZonedDateTime.now(ZoneId.of("UTC")), by = "superadmin")
  }

  def getCurrentZonedDateUTC(): ZonedDateTime = {
    ZonedDateTime.now(ZoneId.of("UTC"))
  }



  def getCurrentDatesAsJson(): String = Json.toJson(getCurrentDatesInUTC()).toString()
  //def getCurrentDatesAsJson(): String = getCurrentDatesInUTC().asJson.toString()

  //Format used: Thu Dec 10 11:04:20 IST 2020
  def toZonedDateTime(date: String, format: String): Option[ZonedDateTime] = {
    val libDateFormat: String = "EEE MMM dd HH:mm:ss z yyyy"
    val df = if (format.isEmpty) DateTimeFormatter.ofPattern(libDateFormat) else DateTimeFormatter.ofPattern(format)
    //val df = if (format.isEmpty) DateTimeFormatter.ISO_ZONED_DATE_TIME else DateTimeFormatter.ofPattern(format)
    try {
      Some(ZonedDateTime.parse(date, df))
    }catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }

  def toLocalDate(date: String, format: String): Option[LocalDate] = {
    //e.g. Sat Nov 30 00:00:00 IST 2019
    val libDateFormat: String = "EEE MMM dd HH:mm:ss z yyyy"
    val df = if (format.isEmpty) DateTimeFormatter.ofPattern(libDateFormat) else DateTimeFormatter.ofPattern(format)
    //val df = if (format.isEmpty) DateTimeFormatter.ISO_ZONED_DATE_TIME else DateTimeFormatter.ofPattern(format)
    try {
      val dtAndTime = ZonedDateTime.parse(date, df)
      Some(LocalDate.of(dtAndTime.getYear, dtAndTime.getMonth, dtAndTime.getDayOfMonth))
    }catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }
}

