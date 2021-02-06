package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.FullJob
import io.circe._
import io.circe.generic.auto._
import io.circe.parser.{decode, _}
import io.circe.syntax._

class FullJobDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[FullJob](db, es) {
  override val collectionName: String = "raw_jds"
  override val collection = db.setupCollection(collectionName, Seq("content.content", "content.name"))
}



object FullJobDAO {
  def saveAll(db: ArangoDbService, es: ElasticsearchService, data: FullJob): Unit = {
    val repo = new FullJobDAO(db, es)
    repo.save(data)
  }
}