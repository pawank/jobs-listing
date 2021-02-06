package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.Employer

class EmployerDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[Employer](db, es) {
  override val collectionName: String = "employers"
  override val collection = db.setupCollection(collectionName, Seq("name", "website", "extra.state", "extra.department"))
}


