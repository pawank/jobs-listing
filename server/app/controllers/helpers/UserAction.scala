package controllers.helpers

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}

import javax.inject.Inject
import models.daos.{ArangoDbUserDAOImpl, AuthTokenDAO, AuthTokenDAOImpl, UserDAO}
import models.{AppUser}
import play.api.mvc._
import services.{ArangoDbService, ElasticsearchService}
import java.util.UUID

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class UserRequest[A](val user: Option[AppUser], request: Request[A]) extends WrappedRequest[A](request)

class UserAction @Inject()(val parser: BodyParsers.Default, db: ArangoDbService,
                           es: ElasticsearchService)(implicit val executionContext: ExecutionContext)
  extends ActionBuilder[UserRequest, AnyContent]
    with ActionTransformer[Request, UserRequest] {
  val sessionDao = new AuthTokenDAOImpl(db, es)
  val userDao = new ArangoDbUserDAOImpl(db, es)
  def transform[A](request: Request[A]) = Future.successful {

    val sessionTokenOpt = request.session.get("sessionToken")
    val apiSessionTokenOpt = request.headers.get("x-auth-token")
    val tokenOpt = if (sessionTokenOpt.isDefined) sessionTokenOpt else apiSessionTokenOpt

    val user = tokenOpt
      .flatMap(tokenMaybe => {
        val tokens = tokenMaybe.split("-token-").tail
          val token = tokens.headOption.getOrElse(UUID.randomUUID().toString)
        val v = sessionDao.find(UUID.fromString(token))
        val session = Await.result(v, Duration.Inf)
        println(s"UserAction token = $token and session = $v")
        session
      })
      .filter(_.expiry.isAfter(ZonedDateTime.now(ZoneOffset.UTC)))
      .map(_.userID)
      .flatMap(uid => {
        val user = userDao.find(uid).map(u => u.map(x => AppUser(x.email, x.userID.toString, x.roles.headOption.getOrElse(""))))
        Await.result(user, Duration.Inf)
      })

    new UserRequest(user, request)
  }
}