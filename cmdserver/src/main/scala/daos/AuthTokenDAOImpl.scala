package models.daos

import java.time.ZonedDateTime
import java.util.concurrent.ExecutionException
import java.util.UUID

import services.ElasticsearchService
//import javax.inject.Inject
import com.google.inject.Inject

import models.AuthToken
import models.daos.AuthTokenDAOImpl._
import models.ModelsUtils._

import scala.collection.mutable
import scala.concurrent.Future

import scala.collection.mutable
import scala.concurrent.Future
import services.ArangoDbService

import scala.compat.java8.FutureConverters._

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._

import com.arangodb.ArangoCursor

import collection.JavaConverters._

/**
 * Give access to the [[AuthToken]] object.
 */
class AuthTokenDAOImpl @Inject() (db: ArangoDbService, es: ElasticsearchService) extends AuthTokenDAO {
  val collectionName: String = "tokens"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  val collection = db.setupCollection(collectionName, Seq.empty)

  /**
   * Finds a token by its ID.
   *
   * @param id The unique token ID.
   * @return The found token or None if no token for the given ID could be found.
   */
  def find(id: UUID): Future[Option[AuthToken]] = {
    val pk = id.toString()
    try {
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p.id == @id RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("id", id)
            .get()
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[AuthToken](c) match {
              case Left(failure) => println("find: Invalid JSON :(")
              case Right(json) => println(s"find: Yay, got some JSON! $json")
            }
            decode[AuthToken](c)
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
      case _@ ex: Throwable =>
        ex.printStackTrace()
        Future.successful(None)
    }
  }

  /**
   * Finds expired tokens.
   *
   * @param dateTime The current date time.
   */
  def findExpired(dateTime: ZonedDateTime): Future[Seq[AuthToken]] = {
    try {
      Future {
        println(s"Expiry date: $dateTime")
        val query =
          s"""FOR p IN $collectionName FILTER p.expiry < @expiry RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put(
              "expiry",
              if (dateTime != null) dateTime.toString()
              else ZonedDateTime.now().toString()
            )
            .get()
        println(s"Q: $query and map: $bindVars")
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[AuthToken](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) =>
                println(s"findExpired: Yay, got some JSON! $json")
            }
            decode[AuthToken](c)
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
      case _@ ex: Throwable =>
        ex.printStackTrace()
        Future.successful(Seq.empty)
    }
  }

  /**
   * Saves a token.
   *
   * @param token The token to save.
   * @return The saved token.
   */
  def save(token: AuthToken) = {
    //tokens += (token.id -> token)
    println(s"Saving token: $token")
    val doc: Json = token.asJson
    try {
      collection
        .insertDocument(printer.print(doc))
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
      case ex: InterruptedException =>
        ex.printStackTrace()
      case ex: Exception =>
        ex.printStackTrace()
      case _@ ex: Throwable =>
        ex.printStackTrace()
    }
    Future.successful(token)
  }

  /**
   * Removes the token for the given ID.
   *
   * @param id The ID for which the token should be removed.
   * @return A future to wait for the process to be completed.
   */
  def remove(id: UUID) = {
    println(s"Removing token $id")
    try {

      val query =
        s"""FOR p IN $collectionName FILTER p.id == @id REMOVE p IN $collectionName LET removed = OLD RETURN removed"""
      val bindVars: java.util.Map[String, Object] =
        new com.arangodb.util.MapBuilder().put("id", id).get()
      val cursor: ArangoCursor[String] =
        db.db.query(query, bindVars, null, classOf[String])
      cursor
        .iterator()
        .asScala
        .map(c => {
          decode[AuthToken](c) match {
            case Left(failure) => println("Invalid JSON :(")
            case Right(json) => println(s"Yay, got some JSON! $json")
          }
        })
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
      case ex: InterruptedException =>
        ex.printStackTrace()
      case ex: Exception =>
        ex.printStackTrace()
      case _@ ex: Throwable =>
        ex.printStackTrace()
    }
    Future.successful(())
  }
}

/**
 * The companion object.
 */
object AuthTokenDAOImpl {

  /**
   * The list of tokens.
   */

  //val tokens: mutable.HashMap[UUID, AuthToken] = mutable.HashMap()
}
