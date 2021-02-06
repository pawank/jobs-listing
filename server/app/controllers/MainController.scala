package controllers

import java.io.File
import java.time.{LocalDateTime, ZoneOffset, ZonedDateTime}
import java.util.UUID

import daos.{BaseDAO, FullJobDAO, JobDAO, PreferenceDAO}
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Inject
import org.webjars.play.WebJarsUtil
import play.api.Configuration
import play.api.i18n.{I18nSupport, Lang, Langs, Messages, MessagesImpl}
import play.api.libs.mailer.{Email, MailerClient}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request, Result}
import services.{ArangoDbService, ElasticsearchService}
import shared.{ID, LoginInfo, User}
import io.circe.generic.auto._
import models.daos.{ArangoDbUserDAOImpl, AuthTokenDAOImpl}
import models.{AppUser}
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.{Await, ExecutionContext, Future}
import play.api.mvc._

import scala.concurrent.duration.Duration
class MainController[T <: ID] @Inject() (
                                   components: ControllerComponents,
                                   db: ArangoDbService,
                                   es: ElasticsearchService,
                                   langs: Langs,
                                   val config: Configuration
                                 )(
                                   implicit
                                   webJarsUtil: WebJarsUtil,
                                   assets: AssetsFinder,
                                   ex: ExecutionContext
                                 ) extends AbstractController(components) with I18nSupport {

  val logger = play.api.Logger(getClass)
  implicit val lang: Lang = langs.availables.head
  implicit val messages: Messages = MessagesImpl(lang, messagesApi)

  val APP_OFFSET = config.get[Int]("app.offset")
  val CACHE_FOLDER = {
	if ((System.getenv("APP_CACHE_FOLDER") != null) && (!System.getenv("APP_CACHE_FOLDER").isEmpty)) {
		System.getenv("APP_CACHE_FOLDER")
	} else {
	config.get[String]("cache.folder.name")
	}
   }
  val CACHE_FILE_PREFIX_FOLDER = config.get[String]("cache.folder.prefix")
  val APP_API_BASE_URL = {
        val x = System.getenv("APP_API_BASE_URL")
        if ((x == null || x.isEmpty)) {
	config.get[String]("app.api.base.url")
        } else x
	}
  val APP_OVERRIDE_GOOGLE_PDF_FETCH = config.get[String]("app.override.google.fetch").equalsIgnoreCase("true")
  val APP_IGNORE_PDF_FROM_SITES = {
    val xs = config.get[String]("app.ignore.sites.pdf").split(",").toList
    xs ++ xs.map(site => site.replaceAll("""\.""", "-"))
  }
  val APP_IGNORE_PDF_FROM_SITES_ORIGINAL = {
    val xs = config.get[String]("app.ignore.sites.pdf").split(",").toList
    xs
  }
  val APP_GOOGLE_SEARCH_API_KEYS: List[String] = {
    if ((System.getenv("APP_GOOGLE_SEARCH_API_KEYS") != null) && (!System.getenv("APP_GOOGLE_SEARCH_API_KEYS").isEmpty)) {
      System.getenv("APP_GOOGLE_SEARCH_API_KEYS").split(",").toList
    } else {
      List("AIzaSyBtJSVQNH9UGuHEBU4Y7snSaZCsyO1yxOE", "AIzaSyAz_Coj6WAciHgKM3woLiLdBbkRpH17p9c")
    }
  }

  private val WithBasicAuth = {
    val user = System.getenv("APP_BASIC_AUTH_USER")
    val pwd = System.getenv("APP_BASIC_AUTH_PASSWORD")
  }

  def pageNo(implicit request: Request[AnyContent]): Int = {
    try {
      val no = app.utils.Utils.decodeQueryString(request.queryString.get("page").flatMap(_.headOption).getOrElse("1")).toInt
      if (no <= 0) 1 else no
    } catch {
      case e: Exception =>
        1
    }
  }
  def offset(implicit request: Request[AnyContent]): Int = try {
    val no = app.utils.Utils.decodeQueryString(request.queryString.get("offset").flatMap(_.headOption).getOrElse(s"$APP_OFFSET")).toInt
    if (no <= 0) APP_OFFSET else no
  } catch {
    case e: Exception =>
      e.printStackTrace()
      APP_OFFSET
  }

  def searchQuery(implicit request: Request[AnyContent]): Option[String] = {
    try {
      val qs = app.utils.Utils.decodeQueryString(request.queryString.get("search").flatMap(_.headOption).getOrElse(""))
      if (qs.isEmpty) None else Some(qs)
    } catch {
      case e: Exception =>
        None
    }
  }

  def getAdContent(implicit request: Request[AnyContent]): Option[String] = {
    try {
      val qs = app.utils.Utils.decodeQueryString(request.queryString.get("fulljob").flatMap(_.headOption).getOrElse(""))
      if (qs.isEmpty) None else Some(qs)
    } catch {
      case e: Exception =>
        None
    }
  }


  def index = Action.async { implicit request: Request[AnyContent] =>
    Future.successful(Ok)
  }
  val sessionDao = new AuthTokenDAOImpl(db, es)
  val userDao = new ArangoDbUserDAOImpl(db, es)

  def isValidLogin(username: String, password: String): Option[User] = {
    //UserDAO.getUser(username).exists(_.password == password)
    val found = userDao.find(loginInfo = LoginInfo(username, password))
    val user = Await.result(found, Duration.Inf)
    user
  }

  def withPlayUser[AU](block: AppUser => Result): EssentialAction = {
    Security.WithAuthentication(extractUser)(user => Action(block(user)))
  }

  def withUser[AU](block: AppUser => Result)(implicit request: Request[AnyContent]): Result = {
    val user = extractUser(request)

    user
      .map(block)
      .getOrElse(Unauthorized(views.html.defaultpages.unauthorized())) // 401, but 404 could be better from a security point of view
  }
  def withUserF[AU](block: AppUser => Future[Result])(implicit request: Request[AnyContent]): Future[Result] = {
    val user = extractUser(request)
    println(s"Extracted User found = $user")
    user
      .map(block)
      .getOrElse(Future.successful(Unauthorized(views.html.defaultpages.unauthorized()))) // 401, but 404 could be better from a security point of view
  }

  def withAdminUserF[AU](block: AppUser => Future[Result])(implicit request: Request[AnyContent]): Future[Result] = {
    val role = "admin"
    val user = extractUser(request)
    println(s"Extracted User found = $user")
    val roleMatched = if (role.isEmpty) true else user.map(_.role.equalsIgnoreCase(role)).getOrElse(false)
    println(s"User $user for role = $role status = $roleMatched")
    val appUser: Option[AppUser] = if (roleMatched) user else None
    appUser
      .map(block)
      .getOrElse(Future.successful(Unauthorized(views.html.defaultpages.unauthorized()))) // 401, but 404 could be better from a security point of view
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

  def extractUser(req: RequestHeader): Option[AppUser] = {
    val sessionTokenOpt = req.session.get("sessionToken")
    val apiSessionTokenOpt = req.headers.get("x-auth-token")
    val tokenOpt = if (sessionTokenOpt.isDefined) sessionTokenOpt else apiSessionTokenOpt
    println(s"Session token found = $tokenOpt")
    val user = tokenOpt
      .flatMap(tokenMaybe => {
        val tokens = tokenMaybe.split("-token-").tail
        val token = tokens.headOption.getOrElse(UUID.randomUUID().toString)
        val v = sessionDao.find(UUID.fromString(token))
        Await.result(v, Duration.Inf)
      })
      .filter(_.expiry.isAfter(ZonedDateTime.now(ZoneOffset.UTC)))
      .map(_.userID)
      .flatMap(uid => {
        //println(s"User ID = $uid for session $sessionTokenOpt for role = $role")
        val user = userDao.find(uid).map(u => u.map(x => AppUser(x.email, x.userID.toString, x.roles.headOption.getOrElse(""))))
        val r = Await.result(user, Duration.Inf)
        r
      })
    user
  }

  def helperDownloadFile(filepath: String, isDecodeUrl: Boolean = false): Result = {
    val user: Option[User] = None
    val cacheFolder = CACHE_FILE_PREFIX_FOLDER
    val filename = if (cacheFolder.isEmpty) filepath else s"""${cacheFolder}/$filepath"""
    val targetFilename = if (filename.isEmpty) "" else {
      if (isDecodeUrl) app.utils.Utils.decodeUrl(s"""$filename""") else s"""$filename"""
    }
    val APP_FETCH_DIRECT_SOURCE_URL = {
      val x = System.getenv("APP_FETCH_DIRECT_SOURCE_URL")
      if (x == null) true else x.toBoolean
    }
    user match {
      case Some(user) =>
        println(s"Showing file - ${targetFilename}")
        new File(targetFilename).exists() match {
          case true =>
            Ok.sendFile(
              content = new java.io.File(targetFilename),
              inline = true
            )
          case false =>
            if (APP_FETCH_DIRECT_SOURCE_URL)
              Ok(filepath)
            else
            NotFound(Messages("file.not.found", filename))
        }
      case _ =>
        println(s"non login: Showing file - ${targetFilename}")
        new File(targetFilename).exists() match {
          case true =>
            Ok.sendFile(
              content = new java.io.File(targetFilename),
              inline = true
            )
          case false =>
            if (APP_FETCH_DIRECT_SOURCE_URL)
              Ok(filepath)
            else
            NotFound(Messages("file.not.found", filename))
        }
    }
  }

  def downloadFile(filename: String) = Action { implicit request =>
    helperDownloadFile(filename)
  }

  def getFileByTagAndId(tag: String, id: String, name: String) = Action.async { implicit request =>
    val _id = shared.KeyID(_key = Some(id))
    val decodedName = app.utils.Utils.decodeUrl(name)
    println(s"ID: ${_id} and name: $decodedName")
    val APP_FETCH_DIRECT_SOURCE_URL = {
      val x = System.getenv("APP_FETCH_DIRECT_SOURCE_URL")
      if (x == null) true else x.toBoolean
    }
    val filename: Future[String] = tag match {
      case "ads" =>
        val repo = new FullJobDAO(db, es).findByID(_id)
        repo.map(r => {
          r match {
            case Some(obj) =>
              if (APP_FETCH_DIRECT_SOURCE_URL)
                obj.source.url.getOrElse("")
              else
                obj.source.path.replaceAll("""/Users/pawan/git/github/pawank/sarkari-portal/pdfcache/""",CACHE_FOLDER)
            case _ =>
              ""
          }
        })
      case "jobs" =>
        val repo = new JobDAO(db, es).findByID(_id)
        repo.map(r => {
          r match {
            case Some(obj) =>
                obj.advPath.getOrElse("").replaceAll("""/Users/pawan/git/github/pawank/sarkari-portal/pdfcache/""",CACHE_FOLDER)
            case _ =>
              ""
          }
        })
      case _ =>
        Future.successful("")
    }
    filename.map(helperDownloadFile(_))
  }



  def saveUserPreference = Action.async(parse.json) { implicit request =>
    val prefDao = new PreferenceDAO(db, es)
    val payload = request.body
    println(s"Received payload = $payload for user preference")
    prefDao.fromJson(payload.toString()) match {
      case Right(obj) =>
        prefDao.save(obj).map(x => Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200))))
      case Left(error) =>
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }

  def makeProperSearchQuery(query: String, status: String): String = {
    val q = if ((query.toLowerCase.contains(" and ")) || query.toLowerCase.contains(" or ")) query else query.split(" ").filter(!_.trim.isEmpty).map(_.trim).mkString(" AND ")
    val finalQ = {
      val tmp = if (status.isEmpty) q else s"""(status:"Live") AND $q"""
      val andreg = "(?i) (AND)?\\s?AND\\s?$".r
      val orreg = "(?i) (OR)?\\s?OR\\s?$".r
      println(andreg.findAllMatchIn(tmp).toList)
      println(orreg.findAllMatchIn(tmp).toList)
      val tmpAnd = andreg.replaceAllIn(tmp,"")
      val tmpOr = orreg.replaceAllIn(tmpAnd,"")
      if (tmpOr.trim.equals("+")) "" else tmpOr.trim
    }
    val result = finalQ
    println(s"Helper jobs search input query = $query, q = $q and Q = $finalQ and final Q = $result")
    result
  }
}

