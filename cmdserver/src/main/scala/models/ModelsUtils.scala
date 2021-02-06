package models

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.util.control.NonFatal
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._
import io.circe.parser._

object ModelsUtils {
  //ZonedDateTime formatter in UTC for auto circe encoding/decoding
  val utcDateFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  implicit val encodeInstant: Encoder[ZonedDateTime] =
    Encoder.encodeString
      .contramap[ZonedDateTime](dt => dt.format(utcDateFormatter))

  implicit val decodeInstant: Decoder[ZonedDateTime] = Decoder.decodeString.emap {
    s =>
      try {
        Right(ZonedDateTime.parse(s, utcDateFormatter))
      } catch {
        case NonFatal(e) => Left(e.getMessage)
      }
  }

  implicit val encodeUUID: Encoder[UUID] =
    Encoder.encodeString
      .contramap[UUID](_.toString())

  implicit val decodeUUID: Decoder[UUID] = Decoder.decodeString.emap { s =>
    try {
      Right(UUID.fromString(s))
    } catch {
      case NonFatal(e) => Left(e.getMessage)
    }
  }
}
