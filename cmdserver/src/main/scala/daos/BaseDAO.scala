package daos

import java.util.UUID
import java.util.concurrent.ExecutionException

import com.arangodb.model.AqlQueryOptions
import com.arangodb.{ArangoCollection, ArangoCursor}
import com.google.inject.Inject
import io.circe._
import io.circe.generic.auto._
import io.circe.parser.{decode, _}
import io.circe.syntax._
import org.elasticsearch.client.ElasticsearchClient
import services.{ArangoDbService, ElasticsearchService}
import shared.ID

import scala.util.Try
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}

/**
  * Give access to the user object.
  */
abstract class BaseDAO[T <: ID] @Inject() (
    db: ArangoDbService,
    es: ElasticsearchService
) {
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  val IS_DEBUG = {
    if (System.getenv("APP_DEBUG_ENABLED") != null) {
      System.getenv("APP_DEBUG_ENABLED").toBoolean
    } else false
  }
  val IS_USE_ES = {
    if (System.getenv("PORTAL_USE_ES") != null) {
      System.getenv("PORTAL_USE_ES").toBoolean
    } else false
  }
  def collectionName: String

  def collection: ArangoCollection

  //implicit val encoder: Encoder[T]
  //implicit val decoder: Decoder[T]

  def fromJson(
      data: String
  )(implicit decoder: Decoder[T]): Either[String, T] = {
    io.circe.parser.parse(data) match {
      case Right(value) =>
        println(s"Value: $value")
        value.as[T] match {
          case Right(obj) =>
            //repo.save(obj).map(x => Ok(x.toString))
            Right(obj)
          case Left(error) =>
            println(error)
            //Future.successful(BadRequest(error.toString()))
            Left(error.toString())
        }
      case Left(errors) =>
        println(errors)
        //Future.successful(BadRequest(errors.toString()))
        Left(errors.toString)
    }
  }

  def getTotalCount(): Int = {
    //collection)
    try {
      val query =
        s"""RETURN LENGTH($collectionName)"""
      val cursor: ArangoCursor[String] =
        db.db.query(query, null, null, classOf[String])
      cursor
        .iterator()
        .asScala
        .map(c => {
          c.toInt
        })
        .toSeq
        .headOption
        .getOrElse(0)
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        0
      case ex: InterruptedException =>
        ex.printStackTrace()
        0
      case ex: Exception =>
        ex.printStackTrace()
        0
      case _ @ex: Throwable =>
        ex.printStackTrace()
        0
    }
  }

  def findByID(id: ID)(implicit decoder: Decoder[T]): Future[Option[T]] = {
    try {
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p._key == @key RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("key", id._key.getOrElse(""))
            .get()
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            /*
            decode[T](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) => println(s"Yay, got some JSON! $json")
            }*/
            decode[T](c)
          })
          .filter(x => x.isRight)
          .map(_.right.get)
          .toSeq
          .headOption
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(None)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(None)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(None)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(None)
    }
  }

  def findByKeyValue(key: String, value: String)(implicit
      decoder: Decoder[T]
  ): Future[Option[T]] = {
    try {
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p.$key == @value RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("value", value)
            .get()
        //println(s"findByKeyValue: Q = $query and bind vars = $bindVars")
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            /*
            decode[T](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) => println(s"Yay, got some JSON! $json")
            }*/
            decode[T](c)
          })
          .filter(x => x.isRight)
          .map(_.right.get)
          .toSeq
          .headOption
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(None)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(None)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(None)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(None)
    }
  }

  def findByQueryForCount(
      keyValueParams: Seq[(String, String)],
      offset: Int,
      count: Int,
      conditionType: String = "AND",
      sortingKey: String = "_key ASC"
  )(implicit decoder: Decoder[T]): Future[Int] = {
    try {
      Future {
        val keyValues = keyValueParams
          .filter(k => !k._1.startsWith("~"))
          .map(k =>
            (
              k._1,
              k._2,
              k._1.replaceAll("""\.""", "") + app.utils.Utils.getUniqueID()
            )
          )
        val keyValuesNeg = keyValueParams
          .filter(k => k._1.startsWith("~"))
          .map(s => (s._1.substring(1), s._2))
          .map(k =>
            (
              k._1,
              k._2,
              k._1.replaceAll("""\.""", "") + app.utils.Utils.getUniqueID()
            )
          )
        println(
          s"findByQuery: keyValues = $keyValues and keyValuesNeg = $keyValuesNeg"
        )
        val filters: String = keyValues
          .map(k => {
            val cond =
              if (k._1.contains(".created")) s"p.${k._1} >= @${k._3}"
              else {
                if (
                  k._1.equalsIgnoreCase("active") || k._1
                    .equalsIgnoreCase("fetchAllParams")
                ) {
                  s"p.${k._1} == @${k._3}"
                } else {
                  s"p.${k._1} == @${k._3}"
                }
              }
            cond
          })
          .mkString(s" $conditionType ")
        val filtersNeg: String = keyValuesNeg
          .map(k => {
            val cond =
              if (k._1.contains(".created")) s"p.${k._1} >= @${k._3}"
              else {
                if (
                  k._1.equalsIgnoreCase("active") || k._1
                    .equalsIgnoreCase("fetchAllParams")
                ) {
                  s"p.${k._1} != @${k._3}"
                } else {
                  s"p.${k._1} != @${k._3}"
                }
              }
            cond
          })
          .mkString(s" $conditionType ")
        val finalFilters =
          if (keyValuesNeg.isEmpty) filters
          else { filters + s" $conditionType " + filtersNeg }
        val query = if (keyValues.isEmpty)
          s"""FOR p IN $collectionName LIMIT $offset, $count SORT p.$sortingKey RETURN p"""
        else s"""FOR p IN $collectionName FILTER $finalFilters LIMIT $offset, $count SORT p.$sortingKey RETURN p"""
        val bindVars: java.util.Map[String, Object] = {
          val mb = new com.arangodb.util.MapBuilder()
          keyValues.map(k => {
            if (
              k._1.equalsIgnoreCase("active") || k._1
                .equalsIgnoreCase("fetchAllParams")
            ) {
              mb.put(k._3, k._2.toBoolean)
            } else {
              mb.put(k._3, k._2)
            }
          })
          keyValuesNeg.map(k => {
            if (
              k._1.equalsIgnoreCase("active") || k._1
                .equalsIgnoreCase("fetchAllParams")
            ) {
              mb.put(k._3, k._2.toBoolean)
            } else {
              mb.put(k._3, k._2)
            }
          })
          mb.get()
        }
        println(
          s"findByQuery: positive filters = $filters and neg fiters = $filtersNeg and Q = $query and bind vars = $bindVars"
        )
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .toSeq
          .size
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(0)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(0)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(0)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(0)
    }
  }

  def findByQuery(
      keyValueParams: Seq[(String, String)],
      offset: Int,
      count: Int,
      conditionType: String = "AND",
      sortingKey: String = "_key ASC"
  )(implicit decoder: Decoder[T]): Future[Seq[T]] = {
    try {
      Future {
        val keyValues = keyValueParams
          .filter(k => !k._1.startsWith("~"))
          .map(k =>
            (
              k._1,
              k._2,
              k._1.replaceAll("""\.""", "") + app.utils.Utils.getUniqueID()
            )
          )
        val keyValuesNeg = keyValueParams
          .filter(k => k._1.startsWith("~"))
          .map(s => (s._1.substring(1), s._2))
          .map(k =>
            (
              k._1,
              k._2,
              k._1.replaceAll("""\.""", "") + app.utils.Utils.getUniqueID()
            )
          )
        println(
          s"findByQuery: keyValues = $keyValues and keyValuesNeg = $keyValuesNeg"
        )
        val filters: String = keyValues
          .map(k => {
            val cond =
              if (k._1.contains(".created") || k._1.contains(".completed")) s"p.${k._1} >= @${k._3}"
              else {
                if (conditionType.equalsIgnoreCase("IN")) {
                    s"p.${k._1} IN @${k._3}"
                } else {
                  if (
                    k._1.equalsIgnoreCase("active") || k._1
                      .equalsIgnoreCase("fetchAllParams")
                  ) {
                    s"p.${k._1} == @${k._3}"
                  } else {
                    s"p.${k._1} == @${k._3}"
                  }
                }
              }
            cond
          })
          .mkString(s" $conditionType ")
        val filtersNeg: String = keyValuesNeg
          .map(k => {
            val cond =
              if (k._1.contains(".created") || k._1.contains(".completed")) s"p.${k._1} >= @${k._3}"
              else {
                if (
                  k._1.equalsIgnoreCase("active") || k._1
                    .equalsIgnoreCase("fetchAllParams")
                ) {
                  s"p.${k._1} != @${k._3}"
                } else {
                  s"p.${k._1} != @${k._3}"
                }
              }
            cond
          })
          .mkString(s" $conditionType ")
        val finalFilters =
          if (keyValuesNeg.isEmpty) filters
          else { filters + s" $conditionType " + filtersNeg }
        val query = if (keyValues.isEmpty)
          s"""FOR p IN $collectionName LIMIT $offset, $count SORT p.$sortingKey RETURN p"""
        else s"""FOR p IN $collectionName FILTER $finalFilters LIMIT $offset, $count SORT p.$sortingKey RETURN p"""
        val bindVars: java.util.Map[String, Object] = {
          val mb = new com.arangodb.util.MapBuilder()
          keyValues.map(k => {
            if (
              k._1.equalsIgnoreCase("active") || k._1
                .equalsIgnoreCase("fetchAllParams")
            ) {
              mb.put(k._3, k._2.toBoolean)
            } else {
                if (conditionType.equalsIgnoreCase("IN")) {
                  //import scala.jdk.CollectionConverters._
                  val xs: java.util.ArrayList[String] = new java.util.ArrayList[String]
                  k._2.split(",").toList.map(data => {
                    xs.add(data)
                    //xs.add(s""""$data"""")
                  })
                  mb.put(k._3, xs)//.map(s => s""""$s""""))
                } else {
                  mb.put(k._3, k._2)
                }
            }
          })
          keyValuesNeg.map(k => {
            if (
              k._1.equalsIgnoreCase("active") || k._1
                .equalsIgnoreCase("fetchAllParams")
            ) {
              mb.put(k._3, k._2.toBoolean)
            } else {
              mb.put(k._3, k._2)
            }
          })
          mb.get()
        }
        println(
          s"findByQuery: positive filters = $filters and neg fiters = $filtersNeg and Q = $query and bind vars = $bindVars"
        )
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[T](c)
          })
          //.map(x => {
          //  println(s"X: ${x.left}")
          //  x
          //})
          .filter(x => x.isRight)
          .map({ x =>
            //println(s"OBJ: ${x.right.get}")
            x.right.get
          })
          .toSeq
        //println(s"No of records found = ${xs.size}")
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
    }
  }

  def searchByQuery(
      keyValueParams: Seq[(String, String)],
      offset: Int,
      count: Int
  )(implicit decoder: Decoder[T]): Future[Seq[T]] = {
    try {
      Future {
        val keyValues =
          keyValueParams.map(k => (k._1, k._2, k._1.replaceAll("""\.""", "")))
        val filters: String = keyValues
          .map(k => {
            val keyAt = k._1.replaceAll("""[^a-zA-Z0-9]+""", "")
            s"p.${k._1} LIKE @${keyAt}"
          })
          .mkString(" OR ")
        val query =
          if (keyValues.isEmpty)
            s"""FOR p IN $collectionName LIMIT $offset, $count RETURN p"""
          else
            s"""FOR p IN $collectionName FILTER $filters LIMIT $offset, $count RETURN p"""
        val bindVars: java.util.Map[String, Object] = {
          val mb = new com.arangodb.util.MapBuilder()
          keyValues.foreach(k => {
            val keyAt = k._1.replaceAll("""[^a-zA-Z0-9]+""", "")
            mb.put(keyAt, s"${k._2}%")
          })
          mb.get()
        }
        println(s"searchByQuery: Q = $query")
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            /*
            decode[T](c) match {
              case Left(failure) =>
                println("Invalid JSON :(")
              case Right(json) =>
                println(s"Yay, got some JSON! $json")
            }*/
            decode[T](c)
          })
          .filter(x => x.isRight)
          .map(_.right.get)
          .toSeq
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
    }
  }

  def getIndexName(): String = {
    collectionName match {
      case "jds"       => "jobs"
      case "raw_jds"   => "ads"
      case "employers" => "employers"
      case "jobs"      => "jobs"
      case "ads"       => "ads"
      case _           => ""
    }
  }

  def search(
      searchClient: ElasticsearchService,
      attribute: String,
      searchQuery: String,
      offset: Int,
      count: Int,
      queries: Seq[String] = Seq.empty
  )(implicit decoder: Decoder[T]): Future[Tuple2[Long, Seq[T]]] = {
    try {
      Future {
        val indexName = getIndexName()
        if (indexName.isEmpty) {
          println(
            s"Search for index: $indexName for collection: $collectionName not supported"
          )
          (0L, Seq.empty)
        } else {
          val tmpQ = searchQuery
            .replaceAll(""" AND """, """ +""")
            .replaceAll(""" OR """, " | ")
            .replaceAll(""" NOT """, """ -""")
            .replaceAll("""NOT """, """ -""")
          val finalQ = {
            val qqq =
              if (tmpQ.trim.startsWith("-")) tmpQ.trim else s"""+$tmpQ"""
            qqq.replaceAll("""\+~""", "-")
          }
          println(
            s"Searching with attribute = $attribute and input q = $searchQuery, queries = $queries and final Q = $finalQ for count = $count and offset = $offset"
          )
          val (totalHits: Long, datas: List[String]) = searchClient.search(
            indexName,
            queries = queries,
            finalQ,
            offset,
            count
          )
          //println(s"totalHits = $totalHits and datas size = ${datas.size}")
          (
            totalHits,
            datas
              .map(c => {
                decode[T](c)
              })
              .filter(x => x.isRight)
              .map(_.right.get)
              .toSeq
          )
        }
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
    }
  }

  def save(value: T, updateES: Boolean = true)(implicit
      encoder: Encoder[T],
      decoder: Decoder[T]
  ): Future[T] = {
    import io.circe.optics.JsonPath._
    if (IS_DEBUG)
      println(s"Saving object: $value")
    val doc: Json = value.asJson
    try {
      val insertDoc: String =
        printer.print(doc) //.replaceAll("""@""", """@@""")
      val updatedDoc: String =
        printer.print(doc) //.replaceAll("""@""", """@@""")
      val query =
        s"""UPSERT { _key: @key }
        INSERT $insertDoc
        REPLACE $updatedDoc IN $collectionName
        RETURN { doc: NEW, action: OLD ? "update" : "insert" }
        """
      //println(s"Q for save: $query")
      val bindVars: java.util.Map[String, Object] =
        new com.arangodb.util.MapBuilder()
          .put("key", value._key.getOrElse(""))
          .get()
      val cursor: ArangoCursor[String] =
        db.db.query(query, bindVars, null, classOf[String])
      val result = cursor
        .iterator()
        .asScala
        .map(c => {
          val parsed = parse(c).getOrElse(Json.Null)
          //println(s"parsed; $parsed")
          val docLens = root.doc.json.getOption(parsed)
          //val actionLens = root.action.string.getOption(parsed)
          //println(s"doc lens: $docLens")
          val parsingResult: Option[T] =
            docLens.getOrElse(Json.Null).as[T] match {
              case Left(failure) =>
                println(s"Invalid JSON: $c")
                None
              case Right(json) =>
                if (IS_DEBUG)
                  println(s"Yay, got some JSON! $json")
                Try {
                  if (IS_USE_ES && updateES) {
                    val indexName = getIndexName()
                    if (value._key.isDefined) {
                      val r = es.updateIndex(
                        indexName,
                        value._key.getOrElse(""),
                        insertJson = insertDoc,
                        updateJson = updatedDoc
                      )
                      //es.refreshIndexes(Seq(indexName))
                      if (IS_DEBUG)
                        println(
                          s"Updated ElasticSearch for index: $indexName with result: $r"
                        )
                    } else {
                      if (IS_DEBUG)
                        println(
                          s"ElasticSearch for index: $indexName cannot be updated without ID"
                        )
                    }
                  }
                }
                Some(json)
            }
          if (parsingResult.isDefined) Right(parsingResult.get)
          else decode[T](c)
        })
        .filter(x => {
          //println(s"x right: ${x}")
          x.isRight
        })
        .map(_.right.get)
        .toSeq
        .headOption
      val finaljd = result
        .map(obj => {
          Try {
            if (IS_USE_ES && updateES) {
              val indexName = getIndexName()
              if (IS_DEBUG)
                println(s"Value key: ${value._key} and obj key: ${obj._key}")
              if (!value._key.isDefined && obj._key.isDefined) {
                val datajson = obj.asJson.toString()
                if (IS_DEBUG)
                  println(s"datajson: $datajson")
                val r = es.updateIndex(
                  indexName,
                  obj._key.getOrElse(""),
                  insertJson = datajson,
                  updateJson = datajson
                )
                //es.refreshIndexes(Seq(indexName))
                if (IS_DEBUG)
                  println(
                    s"Updated ElasticSearch retry for index: $indexName with result: $r"
                  )
              } else {
                if (value._key.isDefined && obj._key.isDefined) {} else {
                  println(
                    s"ERROR: ElasticSearch for index: $indexName cannot be updated without ID"
                  )
                }
              }
            }
          }
          obj
        })
        .get
      Future.successful(finaljd)
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(value)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(value)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(value)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(value)
    }
  }

  def deleteByID(id: ID)(implicit decoder: Decoder[T]): Future[Boolean] = {
    try {
      Future {
        try {
          val query =
            s"""REMOVE '${id._key.getOrElse("")}' IN $collectionName"""
          val cursor: ArangoCursor[String] =
            db.db.query(query, null, null, classOf[String])
          val v = cursor
            .iterator()
            .asScala
          //println(s"v: ${v.toList}")
          if (IS_USE_ES) {
            Try {
              val indexName = getIndexName()
              if (indexName.isEmpty) {
                println(
                  s"Sorry, ElasticSearch Delete not supported for collection: $collectionName for ID: $id"
                )
              } else {
                es.delete(indexName, id._key.getOrElse(""))
                println(
                  s"ElasticSearch index deleted in collection: $collectionName for ID: $id"
                )
              }
            }
          }
          v.isEmpty
        } catch {
          case _ @ex: Throwable =>
            if (
              app.utils.Utils.stackTrace(ex).contains("AQL: document not found")
            ) {
              true
            } else false

        }
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        if (
          app.utils.Utils.stackTrace(ex).contains("AQL: document not found")
        ) {
          Future.successful(true)
        } else Future.successful(false)
      case ex: InterruptedException =>
        ex.printStackTrace()
        if (
          app.utils.Utils.stackTrace(ex).contains("AQL: document not found")
        ) {
          Future.successful(true)
        } else Future.successful(false)
      case ex: Exception =>
        ex.printStackTrace()
        if (
          app.utils.Utils.stackTrace(ex).contains("AQL: document not found")
        ) {
          Future.successful(true)
        } else Future.successful(false)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        if (
          app.utils.Utils.stackTrace(ex).contains("AQL: document not found")
        ) {
          Future.successful(true)
        } else Future.successful(false)
    }
  }

  def batchUpdate(
      keyValueParams: Seq[(String, String)],
      conditionType: String,
      updateParams: (String, String)
  )(implicit decoder: Decoder[T]): Future[Boolean] = {
    try {
      Future {
        val keyValues =
          keyValueParams.map(k => (k._1, k._2, k._1.replaceAll("""\.""", "")))
        val updateKeyValues = (
          updateParams._1,
          updateParams._2,
          updateParams._1.replaceAll("""\.""", "")
        )
        val filters: String = keyValues
          .map(k => {
            val cond =
              if (k._1.contains(".created")) s"p.${k._1} >= @${k._3}"
              else {
                if (
                  k._1.equalsIgnoreCase("active") || k._1
                    .equalsIgnoreCase("fetchAllParams")
                ) {
                  s"p.${k._1} == @${k._3}"
                } else {
                  s"p.${k._1} == @${k._3}"
                }
              }
            cond
          })
          .mkString(s" $conditionType ")
        val query = if (keyValueParams.isEmpty) {
          s"""FOR p IN $collectionName
              UPDATE p WITH { ${updateKeyValues._1}: @${updateKeyValues._3} } IN $collectionName
           """
        } else {
          s"""FOR p IN $collectionName
              FILTER $filters
              UPDATE p WITH { ${updateKeyValues._1}: @${updateKeyValues._3} } IN $collectionName
           """
        }
        //println(s"Q for save: $query")
        val bindVars: java.util.Map[String, Object] = {
          val mb = new com.arangodb.util.MapBuilder()
          keyValues.foreach(k => {
            val keyAt = k._1.replaceAll("""[^a-zA-Z0-9]+""", "")

            if (
              k._1.equalsIgnoreCase("active") || k._1
                .equalsIgnoreCase("fetchAllParams")
            ) {
              mb.put(keyAt, s"${k._2.toBoolean}")
            } else {
              mb.put(keyAt, s"${k._2}")
            }
          })
          if (
            updateKeyValues._1.equalsIgnoreCase("active") || updateKeyValues._1
              .equalsIgnoreCase("fetchAllParams")
          ) {
            mb.put(updateKeyValues._3, s"${updateKeyValues._2.toBoolean}")
          } else {
            mb.put(updateKeyValues._3, s"${updateKeyValues._2}")
          }
          mb.get()
        }
        println(
          s"Batch update: Q = $query and bind vals = ${bindVars.toString}"
        )
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        println(s"Update count = ${cursor.count()}")
        true
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(false)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(false)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(false)
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful(false)
    }
  }

  def searchStates(
      searchClient: ElasticsearchService,
      attribute: String,
      searchQuery: String,
      offset: Int,
      count: Int,
      queries: Seq[String] = Seq.empty
  )(implicit decoder: Decoder[T]): Future[Tuple2[Long, Seq[String]]] = {
    try {
      Future {
        val indexName = getIndexName()
        if (indexName.isEmpty) {
          println(
            s"Search states for index: $indexName for collection: $collectionName not supported"
          )
          (0L, Seq.empty)
        } else {
          val tmpQ = searchQuery
            .replaceAll(""" AND """, """ +""")
            .replaceAll(""" OR """, " | ")
            .replaceAll(""" NOT """, """ -""")
            .replaceAll("""NOT """, """ -""")
          val finalQ = {
            val qqq =
              if (tmpQ.trim.startsWith("-")) tmpQ.trim else s"""+$tmpQ"""
            val x = qqq.replaceAll("""\+~""", "-")
            if (x.equalsIgnoreCase("+")) "" else x
          }
          println(
            s"Searching states with attribute = $attribute and input q = $searchQuery, queries = $queries and final Q = $finalQ for count = $count and offset = $offset"
          )
          val (totalHits: Long, datas: List[String]) = searchClient.search(
            indexName,
            queries = queries,
            finalQ,
            offset,
            count
          )
          (totalHits, datas)
        }
      }
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
      case _ @ex: Throwable =>
        ex.printStackTrace()
        Future.successful((0L, Seq.empty))
    }
  }
}
