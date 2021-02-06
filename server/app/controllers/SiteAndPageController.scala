package controllers

import actors.DataLoaderActor
import akka.actor.{ActorSystem, Props}
import daos.{JobDAO, SiteAndPageDAO}
import io.circe.generic.auto._
import javax.inject.{Inject, Singleton}
import org.webjars.play.WebJarsUtil
import play.api.Configuration
import play.api.i18n.{Lang, Langs, Messages, MessagesImpl}
import play.api.libs.mailer.MailerClient
import play.api.mvc.{AnyContent, ControllerComponents, Request}
import services.{ArangoDbService, ElasticsearchService}
import shared.SitePage

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SiteAndPageController @Inject()(
                               system: ActorSystem,
                                   components: ControllerComponents,
                               langs: Langs,
                               mainTemplate: views.html.base,
                               template: views.html.page,
                               assetsFinder: AssetsFinder,
                               config: Configuration,
                                   db: ArangoDbService,
                               es: ElasticsearchService,
                                   mailerClient: MailerClient
                                 )(
                                   implicit
                                   webJarsUtil: WebJarsUtil,
                                   assets: AssetsFinder,
                                   ex: ExecutionContext
                                 ) extends MainController[SitePage](components, db, es, langs, config) {

  //val logger = play.api.Logger(getClass)
  //implicit val lang: Lang = langs.availables.head
  //implicit val messages: Messages = MessagesImpl(lang, messagesApi)


  val repo = new SiteAndPageDAO(db,es)

  override def index = Action.async { implicit request: Request[AnyContent] =>
    repo.findByKeyValue("siteName", "pages").map(page => Ok(template(page.getOrElse(SitePage.default), assetsFinder)))
  }

  def save = Action.async(parse.json) { implicit request =>
    val payload = request.body
    repo.fromJson(payload.toString()) match {
      case Right(obj) =>
        repo.save(obj).map(x => Ok(play.api.libs.json.Json.toJson(shared.SuccessResponse(200))))
      case Left(error) =>
        println(error)
        Future.successful(BadRequest(play.api.libs.json.Json.toJson(shared.ErrorResponse(200, "Save error", false, Some(error)))))
    }
  }
}

