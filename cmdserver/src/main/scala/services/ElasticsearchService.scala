package services

import java.net.InetAddress

import com.google.inject.Inject

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import javax.inject.Singleton
import org.elasticsearch.action.ActionFuture
import org.elasticsearch.action.admin.indices.refresh.RefreshRequest
import org.elasticsearch.action.delete.DeleteResponse
import org.elasticsearch.action.get.GetResponse
import org.elasticsearch.action.index.{IndexRequest, IndexResponse}
import org.elasticsearch.action.search.{SearchResponse, SearchType}
import org.elasticsearch.action.update.{UpdateRequest, UpdateResponse}
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.client.transport.TransportClient
import org.elasticsearch.common.settings.Settings
import org.elasticsearch.common.transport.TransportAddress
import org.elasticsearch.common.xcontent.XContentType
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.index.reindex.{BulkByScrollResponse, DeleteByQueryAction, DeleteByQueryRequestBuilder}
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.search.SearchHits
import org.elasticsearch.transport.client.PreBuiltTransportClient

import collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.index.query.BoolQueryBuilder

case class ElasticSearchResult(id: String, status: String, message: Option[String])


trait SearchServer {}

@Singleton
class ElasticsearchService @Inject() extends SearchServer {
  import com.typesafe.config.ConfigFactory
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val elasticSearchClusterName: String = ConfigFactory.load().getString("elasticsearch.cluster.name")
  val elasticSearchConnectionUri: String = ConfigFactory.load().getString("elasticsearch.cluster.uri")
  val elasticSearchPort1: Int = ConfigFactory.load().getInt("elasticsearch.cluster.port1")
  val elasticSearchPort2: Int = ConfigFactory.load().getInt("elasticsearch.cluster.port2")
  val elasticSearchPort3: Int = ConfigFactory.load().getInt("elasticsearch.cluster.port3")

  val settings = Settings.builder()
    //.put("client.transport.sniff", true)
    .put("cluster.name", elasticSearchClusterName).build()
  val client: TransportClient = new PreBuiltTransportClient(settings)
    .addTransportAddress(new
        TransportAddress(InetAddress.getByName("localhost"), 9300))
  .addTransportAddress(new
      TransportAddress(InetAddress.getByName("localhost"), 9302))
    .addTransportAddress(new
        TransportAddress(InetAddress.getByName("localhost"), 9303))
  //val client: TransportClient = new PreBuiltTransportClient(Settings.EMPTY)
    //.addTransportAddress(new TransportAddress(InetAddress.getByName("localhost"), 9300))
    //.addTransportAddress(new TransportAddress(InetAddress.getByName("localhost"), 9300))


  def getIndexType(indexName: Option[String]): String = {
    val collectionName = indexName.getOrElse("unknown")
    collectionName match {
      case "jds" => "jobs"
      case "raw_jds" => "ads"
      case "employers" => "employers"
      case "jobs" => "jobs"
      case "ads" => "ads"
      case _ => collectionName
    }
  }


  def get(indexName: String, key: String): ElasticSearchResult = {
        val response: GetResponse = client.prepareGet(indexName, getIndexType(Some(indexName)), key).get()
      if ((response.getId != null) && (response.getId.equals(key)))
        ElasticSearchResult(id = key, status = "", message = Some(response.getSourceAsString))
      else
      ElasticSearchResult(id = key, status = "Error", message = None)
    }

    def save(indexName: String, dataJson: String, key: Option[String]): ElasticSearchResult = {
      val response: IndexResponse = client.prepareIndex(indexName, getIndexType(key), key.getOrElse(""))
        .setSource(dataJson, XContentType.JSON)
        .get()
      val status: RestStatus = response.status()
      ElasticSearchResult(id = response.getId, status = status.name(), message = Some(response.toString))
    }

  def delete(indexName: String, key: String): ElasticSearchResult = {
    val response: DeleteResponse = client.prepareDelete(indexName, getIndexType(Some(indexName)), key).get()
    ElasticSearchResult(id = key, status = response.status().name(), message = Some(response.toString))
  }

  def deleteByQuery(indexName: String, key: String, params: Seq[(String, String)]): ElasticSearchResult = {
    val response: BulkByScrollResponse =
      new DeleteByQueryRequestBuilder(client, DeleteByQueryAction.INSTANCE)
        .filter(QueryBuilders.matchQuery(params.head._1, params.head._2))
        .source(indexName)
        .get()
    val status = response.getDeleted()
    ElasticSearchResult(id = key, status = if (status > 0) "" else response.getStatus.toString, message = Some(response.toString))
  }
  def updateIndex(indexName: String, key: String, insertJson: String, updateJson: String): ElasticSearchResult = {
    val req: IndexRequest = new IndexRequest(indexName, getIndexType(Some(indexName)), key).source(insertJson, XContentType.JSON)
    val updateReq: UpdateRequest = new UpdateRequest(indexName, getIndexType(Some(indexName)), key)
      .doc(updateJson, XContentType.JSON).upsert(req)
    val response: UpdateResponse = client.update(updateReq).get()
    ElasticSearchResult(id = key, status = response.status().name(), message = Some(response.toString))
  }

  def refreshIndexes(indexes: Seq[String]) = {
    val requestMultiple = new RefreshRequest(indexes: _*)
    client.admin().indices().refresh(requestMultiple)
  }

  def search(indexName: String, queries: Seq[String], searchQuery: String, page: Int, offset: Int) = {
    val query = queries.mkString(" AND ")
    val qt0 = if (queries.isEmpty) QueryBuilders.simpleQueryStringQuery(searchQuery) else QueryBuilders.matchPhraseQuery(queries(0), queries(1))
    //val plusCount = searchQuery.sliding("+".length).count(window => window == "+")
    //val qt = if (searchQuery.contains("status") && searchQuery.contains("Live")) QueryBuilders.queryStringQuery(searchQuery).allowLeadingWildcard(true).defaultField("AND") else qt0
    val sourceBuilder: SearchSourceBuilder = new SearchSourceBuilder()
    val boolQueryBuilder = new BoolQueryBuilder()
    boolQueryBuilder.must(QueryBuilders.queryStringQuery(searchQuery).defaultField("*").analyzeWildcard(true))
    sourceBuilder.query(boolQueryBuilder)
    val qt = sourceBuilder.query()
    val finalPageNo = if (page <= 0) 0 else page
    println(s"ElasticSearch Service search /$indexName with Q: $qt with page: $page (final page: $finalPageNo) and offset: $offset")
    /*
    val encUrl = s"http://localhost:9200/$indexName/_search"
    import ujson._
    //val payload = Js.Obj( "query" -> Js.Obj("bool" -> Js.Obj("must" -> Js.Arr(Js.Obj("query_string" -> searchQuery, "analyze_wildcard" -> true)))))
    //val data = ujson.read(payload)
    //val result = requests.get(encUrl, data = data.render(), readTimeout = api.SiteParserAndCrawler.READ_TIMEOUT, connectTimeout = api.SiteParserAndCrawler.CONNECT_TIMEOUT, verifySslCerts = false)
    val params = Map("q" -> searchQuery)
    val result = requests.get(encUrl, params = params, readTimeout = api.SiteParserAndCrawler.READ_TIMEOUT, connectTimeout = api.SiteParserAndCrawler.CONNECT_TIMEOUT, verifySslCerts = false)
    println(s"RESULT: $result")
    println(s"QUERY = $params")
    */
    val response: SearchResponse = client.prepareSearch(indexName)
      .setQuery(qt)
      .setFrom(finalPageNo).setSize(offset).setExplain(true)
      //.addStoredField("ALL")
      .get()
    val hits: SearchHits = response.getHits
    //println(s"Hits: ${hits.getTotalHits}")
    (hits.getTotalHits.value, hits.iterator().asScala.map(_.getSourceAsString).toList)
  }

  def shutdown() = {
    client.close()
  }
}

