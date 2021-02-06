package daos

import com.google.inject.Inject
import services.{ArangoDbService, ElasticsearchService}
import shared.Education

class EducationDAO @Inject()(db: ArangoDbService, es: ElasticsearchService) extends BaseDAO[Education](db, es) {
  override val collectionName: String = "education_skills"
  override val collection = db.setupCollection(collectionName, Seq.empty)
}


