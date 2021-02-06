package shared

import julienrf.json.derived
import play.api.libs.json.{Format, Json, OFormat}
import shared.JavaReads._
import shared.JavaWrites._

case class Pagination(_id: Option[String] = None, _key: Option[String] = None, page: Int, offset: Int, total: Int, countMessage: String = "") {
  val pageCount = if (offset > 0) {
    if (total > offset)
      scala.math.ceil(total * 1.0 / (offset * 1.0)).toInt
    else 1
  } else 1

  val displayMessage = s"Showing $countMessage matches"
  override def toString: String = s"Page no: $page with offset: $offset and total pages: $pageCount and total count: $total"
}

object Pagination {
  implicit val jsonFormatPagination: Format[Pagination] = Json.format[Pagination]
}

