package shared


import java.util.UUID

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
case class Login(id: Option[String], _key: Option[String], email: String, password: String) extends ID {
}
object Login{
  implicit val jsonFormatLogin: Format[Login] = Json.format[Login]
}


case class LoginInfo(providerID: String, providerKey: String)
object LoginInfo {
  implicit val jsonFormatLoginInfo: Format[LoginInfo] = Json.format[LoginInfo]
}
case class Profile(email: String, firstName: String,
                    lastName: String,
                    phone: Option[String]
                   )
object Profile {
  implicit val jsonFormatUserInfoProfile: Format[Profile] = Json.format[Profile]
}


case class UserInfo(firstName: String,
                    lastName: String,
                    phoneNo: Option[String],
                    dates: DatesInfo
                   )
object UserInfo {
  implicit val jsonFormatUserInfo: Format[UserInfo] = Json.format[UserInfo]
}

case class User(userID: UUID,
                email: String,
                roles: Set[String],
                loginInfo: LoginInfo,
                info: UserInfo,
                likes: List[String] = List.empty,
                id: Option[String] = None,
               _key: Option[String] = None
               ) extends ID {
}

object User {
  implicit val jsonFormatUser: Format[User] = Json.format[User]
}

