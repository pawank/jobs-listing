package daos

import java.util.UUID
import java.util.concurrent.ExecutionException

import com.arangodb.ArangoCursor
import com.google.inject.Inject
import io.circe._
import io.circe.generic.auto._
import io.circe.parser.{decode, _}
import io.circe.syntax._
import services.{ArangoDbService, ElasticsearchService}
import shared.Job

import scala.collection.JavaConverters._
import scala.concurrent.Future

class JobDAO @Inject() (db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[Job](db, es) {
  override val collectionName: String = "jds"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


