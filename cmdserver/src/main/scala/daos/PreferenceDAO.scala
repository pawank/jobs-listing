package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.{Preference}

class PreferenceDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[Preference](db, es) {
  override val collectionName: String = "preferences"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


