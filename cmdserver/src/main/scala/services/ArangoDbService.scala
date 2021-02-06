package services

import java.util

import com.google.inject.Inject

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import javax.inject.Singleton
import com.arangodb.{ArangoCollection, ArangoDB, ArangoDatabase, ArangoSearch}
import com.arangodb.entity.ViewType
import com.arangodb.entity.arangosearch.{CollectionLink, FieldLink}
import com.arangodb.model.FulltextIndexOptions
import com.arangodb.model.arangosearch.{ArangoSearchCreateOptions, ArangoSearchPropertiesOptions}

import collection.JavaConverters._

trait DbServer {}

@Singleton
class ArangoDbService @Inject() extends DbServer {
  import com.typesafe.config.ConfigFactory
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  //Profig.load(ProfigLookupPath("config.json", FileType.Json, LoadType.Merge))

  val hostname: String =
    ConfigFactory.load().getString("arangodb.hostname")
  val port: Int =
    ConfigFactory.load().getInt("arangodb.port")
  val dbName: String =
    ConfigFactory.load().getString("arangodb.db")
  val username: String =
    ConfigFactory.load().getString("arangodb.user")
  private[this] val password: String =
    ConfigFactory.load().getString("arangodb.password")

  //println(s"ArangoDbRepoService: db = $dbName, username = $username")
  //val db = new ArangoDB()
  val arangoDB: ArangoDB = new ArangoDB.Builder()
    .host(hostname, port)
    //.useProtocol(Protocol.HTTP_JSON)
    .user(username)
    .password(password)
    .build()

  def setup() = {
    if (arangoDB.db(dbName).exists()) {
      //arangoDB.db(dbName).drop()
    } else {
      arangoDB.createDatabase(dbName)
    }
    arangoDB.db(dbName)
  }

  val db: ArangoDatabase = setup()

  def close(): Unit = {
    arangoDB.shutdown()
  }

  def setupCollection(collectionName: String, fieldsForFulltext: Seq[String]): ArangoCollection = {
    val matchedColls = db.getCollections().iterator().asScala.toList.map(x => x.getName)
    //println(s"Collections: $matchedColls")
    val coll: ArangoCollection = if (matchedColls.contains(collectionName)) {
      db.collection(collectionName)
    } else {
      //println(s"Creating collection: $collectionName")
      db.createCollection(collectionName)
      //println(s"Loading created collection: $collectionName")
      db.collection(collectionName)
    }
    coll
  }
}
