package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.{JobSession}

class JobSessionDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[JobSession](db, es) {
  override val collectionName: String = "sessions"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


