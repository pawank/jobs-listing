package shared

trait ID {
  def id: Option[String]
  def _key: Option[String]
}

case class KeyID(_key: Option[String], id: Option[String] = None) extends ID