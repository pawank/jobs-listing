package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.SiteCrawler
import io.circe._
import io.circe.generic.auto._
import io.circe.parser.{decode, _}
import io.circe.syntax._

import scala.concurrent.Future

class SiteCrawlerDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[SiteCrawler](db, es) {
  override val collectionName: String = "crawlingsites"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


object SiteCrawlerDAO {
  def saveAll(db: ArangoDbService, es: ElasticsearchService, data: SiteCrawler): Future[SiteCrawler] = {
    val repo = new SiteCrawlerDAO(db, es)
    repo.save(data)
  }
}
