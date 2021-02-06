package modules

import com.google.inject.AbstractModule
import models.daos.{ArangoDbUserDAOImpl, UserDAO}
import net.codingwell.scalaguice.ScalaModule
import services.{ArangoDbService, DbServer, ElasticsearchService, SearchServer}


class ServerModule extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind[DbServer].to[ArangoDbService].asEagerSingleton()
    bind[SearchServer].to[ElasticsearchService].asEagerSingleton()
    bind[UserDAO].to[ArangoDbUserDAOImpl]
  }
}

