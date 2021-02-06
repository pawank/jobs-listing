package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.MatchingRule

class RuleDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[MatchingRule](db, es) {
  override val collectionName: String = "rules"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


