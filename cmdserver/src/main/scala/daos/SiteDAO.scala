package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.{Page, SitePage}

class SiteAndPageDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[SitePage](db, es) {
  override val collectionName: String = "pages"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


