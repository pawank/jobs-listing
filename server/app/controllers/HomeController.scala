package controllers

import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import java.util.UUID

import javax.inject._
import actors._
import akka.NotUsed
import akka.actor._
import akka.pattern.ask
import akka.stream.scaladsl._
import akka.util.Timeout
import controllers.helpers.UserAction
import daos.{EducationDAO, FullJobDAO, JobDAO, RuleDAO, SiteAndPageDAO}
import play.api.i18n.{Lang, Langs, Messages, MessagesImpl}
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Logger}
import services.{ArangoDbService, ElasticsearchService}
import shared.{DatesInfo, Education, LoginInfo, MatchingRule, NameValue, Profile, SitePage, User, UserInfo}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import io.circe.generic.auto._
import models.daos.{ArangoDbUserDAOImpl, AuthTokenDAOImpl}
import models.{AppUser, AuthToken}
/**
  * This class creates the actions and the websocket needed.
  * Original see here: https://github.com/playframework/play-scala-websocket-example
  */
@Singleton
class HomeController @Inject()(cc: ControllerComponents
  ,langs: Langs
, logsTemplate: views.html.logs
                               , template: views.html.index,
                              appView: views.html.app,
                                aboutView: views.html.about_us,
                               privacyView: views.html.privacy
                               , assetsFinder: AssetsFinder
                              , db: ArangoDbService,
                              es: ElasticsearchService
                              , val config: Configuration,
                               val userAction: UserAction
                              )
                              (implicit ec: ExecutionContext)
  extends AbstractController(cc) with SameOriginCheck {

  val logger = play.api.Logger(getClass)
  implicit val lang: Lang = langs.availables.head
  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  val sitePage = new SiteAndPageDAO(db, es)

  // Home page that renders template
  def index = Action.async { implicit request: Request[AnyContent] =>
    val dao = new FullJobDAO(db, es)
    val repo = new JobDAO(db, es)
    val totals = repo.getTotalCount()
    val showLiveJobs = "Live"
    val edus = new EducationDAO(db, es).findByQuery(Seq.empty, 0, 999)
    val rules = new RuleDAO(db, es).findByQuery(Seq(("matchType", "Education")), 0, 999)
    val pageview = sitePage.findByKeyValue("siteName", "home")
      val result: Future[(Option[SitePage], Seq[Education], Long)] = for {
        totals <- repo.findByQuery(Seq(("status", showLiveJobs)), 0, 99999).map(_.size.toLong)
        pv <- pageview
        matches <- edus
      } yield {
        (pv, matches, totals)
        }
    // uses the AssetsFinder API
    result.map(page => {
      println(s"Page found: ${page._1}")
      val jobsCount = page._3
      val dynamicBanner = s"$totals Notifications $jobsCount Jobs and Growing"
      val cats = page._2.groupBy(m => m.category)
      val names = cats.map(xs => NameValue(xs._1.getOrElse(""), xs._2.map(_.name).distinct.mkString(","))).toSeq
      Ok(template(page._1.getOrElse(SitePage.default), assetsFinder, names, dynamicBanner))
    })
  }

  def app = Action.async { implicit request: Request[AnyContent] =>
    // uses the AssetsFinder API
    sitePage.findByKeyValue("siteName", "app").map(page => {
      println(s"Page found: $page")
      Ok(appView(page.getOrElse(SitePage.default), assetsFinder))
    })
  }
  def privacy = Action.async { implicit request: Request[AnyContent] =>
    // uses the AssetsFinder API
    sitePage.findByKeyValue("siteName", "privacy").map(page => {
      println(s"Page found: $page")
      Ok(privacyView(page.getOrElse(SitePage.default).copy(slug = Some("/privacy")), assetsFinder))
    })
  }


  def aboutUs = Action.async { implicit request: Request[AnyContent] =>
    // uses the AssetsFinder API
    sitePage.findByKeyValue("siteName", "app").map(page => {
      println(s"Page found: $page")
      Ok(aboutView(page.getOrElse(SitePage.default).copy(slug = Some("/about-us")), assetsFinder))
    })
  }

  def logs = Action { implicit request: Request[AnyContent] =>
    // uses the AssetsFinder API
    Ok(logsTemplate(assetsFinder))
  }
  val sessionDao = new AuthTokenDAOImpl(db, es)
  val userDao = new ArangoDbUserDAOImpl(db, es)

  private def isValidLogin(username: String, password: String): Option[User] = {
    val found = userDao.find(loginInfo = LoginInfo(username, password))
    val user = Await.result(found, Duration.Inf)
    //user.isDefined
    //UserDAO.getUser(username).exists(_.password == password)
    user
  }

  private def withPlayUser[T](block: AppUser => Result): EssentialAction = {
    Security.WithAuthentication(extractUser)(user => Action(block(user)))
  }

  private def withUser[T](block: AppUser => Result)(implicit request: Request[AnyContent]): Result = {
    val user = extractUser(request)

    user
      .map(block)
      .getOrElse(Unauthorized(views.html.defaultpages.unauthorized())) // 401, but 404 could be better from a security point of view
  }

  private def extractUser(req: RequestHeader): Option[AppUser] = {

    val sessionTokenOpt = req.session.get("sessionToken")
    val apiSessionTokenOpt = req.headers.get("x-auth-token")
    val tokenOpt = if (sessionTokenOpt.isDefined) sessionTokenOpt else apiSessionTokenOpt
    val user = tokenOpt
      .flatMap(tokenMaybe => {
        val tokens = tokenMaybe.split("-token-").tail
        val token = tokens.headOption.getOrElse(UUID.randomUUID().toString)
        val v = sessionDao.find(UUID.fromString(token))
        val session = Await.result(v, Duration.Inf)
        println(s"API: Session token found = $token and session = $session")
        session
      })
      .filter(x => {
        println(s"Expiry = ${x.expiry}")
        x.expiry.isAfter(ZonedDateTime.now(ZoneOffset.UTC))
      })
      .map(_.userID)
      .flatMap(uid => {
        val userF = userDao.find(uid).map(u => u.map(x => AppUser(x.email, x.userID.toString, x.roles.headOption.getOrElse(""))))
        val user = Await.result(userF, Duration.Inf)
        println(s"Found user from request header = $user")
        user
      })
    user
  }

  def login(username: String, password: String) = Action { implicit request: Request[AnyContent] =>
    println(s"Trying to login using username = $username")
    val user = isValidLogin(username, password)
    if (user.isDefined) {
      //val token = SessionDAO.generateToken(username)
      val id = UUID.randomUUID()
      val token = s"$username-token-${id.toString}"
      //val token = id.toString
      val authtoken = AuthToken(id = id, userID = user.get.userID, expiry = ZonedDateTime.now().plusYears(1))
      Await.result(sessionDao.save(authtoken), Duration.Inf)
      println(s"Login ok for username = $username with token = $token")
      Redirect(routes.HomeController.index()).withSession(request.session + ("sessionToken" -> token))
    } else {
      // we should redirect to login page
      Unauthorized(views.html.defaultpages.unauthorized()).withNewSession
    }
  }

  def doLogin = Action.async(parse.json) { implicit request =>
    try {
      val payload = request.body
      val username = (payload \ "username").as[String]
      val password = (payload \ "password").as[String]
      println(s"Trying to login using username = $username")
      val user = isValidLogin(username, password)
      if (user.isDefined) {
        //val token = SessionDAO.generateToken(username)
        val id = UUID.randomUUID()
        val token = s"$username-token-${id.toString}"
        //val token = id.toString
        val authtoken = AuthToken(id = id, userID = user.get.userID, expiry = ZonedDateTime.now().plusYears(1))
        sessionDao.save(authtoken).map(s => {
          println(s"Login ok for username = $username with token = $token")
          Ok(Json.toJson(shared.SuccessResponse(code = 200, message = "", session = Some(token))))
        })
      } else {
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Invalid Login. Please try again.", exception = Some(s"$payload")))))
      }
    } catch {
      case _ : Throwable =>
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Login error has occurred.", exception = Some("JSON error")))))
    }
  }


  def signup = Action.async(parse.json) { implicit request =>
    try {
      val payload = request.body
      val username = (payload \ "username").as[String]
      val password = (payload \ "password").as[String]
      println(s"Trying to login using username = $username")
      val loginInfo = LoginInfo(username, password)
      userDao.find(loginInfo).map(userF => {
        userF match {
          case Some(found) =>
            if (found._key.isDefined) Ok(Json.toJson(shared.SuccessResponse(code = 200, message = s"Account already exists for username, $username"))) else
              Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Account error has occurred.", exception = Some("ID cannot be found."))))
          case _ =>
            val user = User(userID = UUID.randomUUID(), email = username, roles = Set("user"), loginInfo = loginInfo, info = UserInfo(firstName = "", lastName = "", phoneNo = None, dates = DatesInfo.getCurrentDatesInUTC()))
            val u = Await.result(userDao.save(user), Duration.Inf)
            Ok(Json.toJson(shared.SuccessResponse(code = 200, message = s"")))
        }
      })
    } catch {
      case _ : Throwable =>
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Account error has occurred with the requested parameters.", exception = Some("JSON error")))))
    }
  }

  def withUserJsonF[AU](block: AppUser => Future[Result])(implicit request: Request[JsValue]): Future[Result] = {
    val user = extractUser(request)
    println(s"Extracted User found = $user")
    import play.api.libs.json.{Json => PlayJson}
    val errorResult = Unauthorized(PlayJson.toJson(shared.ErrorResponse(code = 403, message = s"You are not authorized to perform the action.", exception = Some(""))))
    user
      .map(block)
      .getOrElse(Future.successful(errorResult)) // 401, but 404 could be better from a security point of view
  }

  def updateUserProfile = Action.async(parse.json) { implicit request =>
    try {
      val raw = request.body
      println(s"API: Updating user profile with payload = $raw")
      withUserJsonF(user => {
        val payload = raw.validate[Profile]
        payload match {
          case JsSuccess(value, path) =>
            userDao.find(value.email).map(userF => {
              userF match {
                case Some(found) =>
                  val profile = found.copy(info = UserInfo(firstName = value.firstName, lastName = value.lastName, phoneNo = value.phone, dates = DatesInfo.getCurrentDatesInUTC()))
                  val v = Await.result(userDao.save(profile), Duration.Inf)
                  println(s"API: Profile update done for $value")
                  Ok(Json.toJson(shared.SuccessResponse(code = 200, message = s"")))
                case _ =>
                  println(s"API: Profile update error = Account not found with the email")
                  Ok(Json.toJson(shared.SuccessResponse(code = 404, message = s"Account not found with the email, ${value.email}")))
              }
            })
          case JsError(errors) =>
            println(s"API: Profile update error = $errors")
            Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Account update error.", exception = Some(errors.toString())))))
        }
      })
    } catch {
      case _ : Throwable =>
        println(s"Account error has occurred with the requested parameters.")
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Account error has occurred with the requested parameters.", exception = Some("JSON error")))))
    }
  }

  def getUserProfile = Action.async(parse.json) { implicit request =>
    try {
      val payload = request.body
      println(s"API: User profile payload = $payload")
      withUserJsonF(user => {
        val username = (payload \ "username").as[String]
        println(s"Trying to get profile using username = $username")
        userDao.find(user.username).map(userF => {
          userF match {
            case Some(found) =>
              val profile = Profile(email = found.email, firstName = found.info.firstName, lastName = found.info.lastName, phone = found.info.phoneNo)
              Ok(Json.toJson(profile))
            case _ =>
              println(s"API: Profile not found with the email, $username")
              Ok(Json.toJson(shared.SuccessResponse(code = 404, message = s"Account not found with the email, ${username}")))
          }
        })
      })
    } catch {
      case _: Throwable =>
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"", exception = Some("")))))
    }
  }


  def logout() = Action { implicit request: Request[AnyContent] =>
    Redirect(routes.HomeController.index()).withNewSession
  }


  private def removeApiSession(req: RequestHeader): Boolean = {
    val sessionTokenOpt = req.session.get("sessionToken")
    val apiSessionTokenOpt = req.headers.get("x-auth-token")
    val tokenOpt = if (sessionTokenOpt.isDefined) sessionTokenOpt else apiSessionTokenOpt
    val tokenMaybe = tokenOpt.getOrElse("")
    println(s"App logout session found = $tokenMaybe")
    if (!tokenMaybe.isEmpty) {
      val tokens = tokenMaybe.split("-token-").tail
      val token = tokens.headOption.getOrElse(UUID.randomUUID().toString)
      val v = sessionDao.remove(UUID.fromString(token))
      val session = Await.result(v, Duration.Inf)
      session
    }
    true
  }

  def doLogout = Action.async(parse.json) { implicit request =>
    try {
      val status = removeApiSession(request)
      Future.successful(Ok(Json.toJson(shared.SuccessResponse(code = 200, message = s""))))
    } catch {
      case _ : Throwable =>
        Future.successful(Ok(Json.toJson(shared.ErrorResponse(code = 400, message = s"Logout error has occurred with the requested parameters.", exception = Some("JSON error")))))
    }
  }


  /**
    * Creates a websocket.  `acceptOrResult` is preferable here because it returns a
    * Future[Flow], which is required internally.
    *
    * @return a fully realized websocket.
    */
  def ws: WebSocket = WebSocket.acceptOrResult[JsValue, JsValue] {
    case rh if sameOriginCheck(rh) =>
      wsFutureFlow(rh).map { flow =>
        Right(flow)
      }.recover {
        case e: Exception =>
          logger.error("Cannot create websocket", e)
          val jsError = Json.obj("error" -> "Cannot create websocket")
          val result = InternalServerError(jsError)
          Left(result)
      }

    case rejected =>
      logger.error(s"Request $rejected failed same origin check")
      Future.successful {
        Left(Forbidden("forbidden"))
      }
  }

  /**
    * Creates a Future containing a Flow of JsValue in and out.
    */
  private def wsFutureFlow(request: RequestHeader): Future[Flow[JsValue, JsValue, NotUsed]] = {
    // Use guice assisted injection to instantiate and configure the child actor.
    implicit val timeout: Timeout = Timeout(1.second) // the first run in dev can take a while :-(
    val future: Future[Any] = Future.successful(0.asInstanceOf[Any])
    val futureFlow: Future[Flow[JsValue, JsValue, NotUsed]] = future.mapTo[Flow[JsValue, JsValue, NotUsed]]
    futureFlow
  }

}

trait SameOriginCheck {

  def logger: Logger

  def config: Configuration

  /**
    * Checks that the WebSocket comes from the same origin.  This is necessary to protect
    * against Cross-Site WebSocket Hijacking as WebSocket does not implement Same Origin Policy.
    *
    * See https://tools.ietf.org/html/rfc6455#section-1.3 and
    * http://blog.dewhurstsecurity.com/2013/08/30/security-testing-html5-websockets.html
    */
  def sameOriginCheck(rh: RequestHeader): Boolean = {
    rh.headers.get("Origin") match {
      case Some(originValue) if originMatches(originValue) =>
        logger.debug(s"originCheck: originValue = $originValue")
        true

      case Some(badOrigin) =>
        logger.error(s"originCheck: rejecting request because Origin header value $badOrigin is not in the same origin")
        false

      case None =>
        logger.error("originCheck: rejecting request because no Origin header found")
        false
    }
  }

  /**
    * Returns true if the value of the Origin header contains an acceptable value.
    *
    * see application.conf: wsocket.hosts.allowed for a description.
    */
  def originMatches(origin: String): Boolean = {
    import scala.collection.JavaConverters._

    val allowedHosts = config.underlying.getStringList("wsocket.hosts.allowed").asScala
    allowedHosts.exists(origin.endsWith)
  }

}
