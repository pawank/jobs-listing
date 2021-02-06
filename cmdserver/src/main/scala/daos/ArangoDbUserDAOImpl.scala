package models.daos

import com.google.inject.Inject
import java.util.concurrent.ExecutionException
import java.util.UUID

import models.ModelsUtils._

import scala.collection.mutable
import scala.concurrent.Future
import services.{ArangoDbService, ElasticsearchService}

import scala.compat.java8.FutureConverters._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._
import io.circe.parser._
import com.arangodb.ArangoCursor
import shared.{LoginInfo, User}

import collection.JavaConverters._

/**
 * Give access to the user object.
 */
class ArangoDbUserDAOImpl @Inject() (db: ArangoDbService, es: ElasticsearchService) extends UserDAO {
  val collectionName: String = "users"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  val collection = db.setupCollection(collectionName, Seq.empty)

  /**
   * Finds a user by its login info.
   *
   * @param loginInfo The login info of the user to find.
   * @return The found user or None if no user for the given login info could be found.
   */
  def find(loginInfo: LoginInfo): Future[Option[User]] = {
    //Future.successful(users.find { case (_, user) => user.loginInfo == loginInfo }.map(_._2))
    try {
      println(s"LoginInfo: $loginInfo")
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p.loginInfo.providerID == @providerID AND p.loginInfo.providerKey == @providerKey RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("providerID", loginInfo.providerID)
            .put("providerKey", loginInfo.providerKey)
            .get()
        println(s"Q: $query and map: $bindVars")
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[User](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) => println(s"Yay, got some JSON! $json")
            }
            decode[User](c)
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

  def find(email: String): Future[Option[User]] = {
    try {
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p.email == @email RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("email", email)
            .get()
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[User](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) => println(s"Yay, got some JSON! $json")
            }
            decode[User](c)
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
   * Finds a user by its user ID.
   *
   * @param userID The ID of the user to find.
   * @return The found user or None if no user for the given ID could be found.
   */
  def find(userID: UUID): Future[Option[User]] = {
    try {
      Future {
        val query =
          s"""FOR p IN $collectionName FILTER p.userID == @userID RETURN p"""
        val bindVars: java.util.Map[String, Object] =
          new com.arangodb.util.MapBuilder()
            .put("userID", userID)
            .get()
        val cursor: ArangoCursor[String] =
          db.db.query(query, bindVars, null, classOf[String])
        cursor
          .iterator()
          .asScala
          .map(c => {
            decode[User](c) match {
              case Left(failure) => println("Invalid JSON :(")
              case Right(json) => println(s"Yay, got some JSON! $json")
            }
            decode[User](c)
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

  def add(user: User): Future[User] = {
    println(s"Adding user: $user")
    val doc: Json = user.asJson
    try {
      val _key = collection
        .insertDocument(printer.print(doc))
        .getKey()
      Future.successful(user.copy(_key = Some(_key)))
    } catch {
      case ex: ExecutionException =>
        ex.printStackTrace()
        Future.successful(user)
      case ex: InterruptedException =>
        ex.printStackTrace()
        Future.successful(user)
      case ex: Exception =>
        ex.printStackTrace()
        Future.successful(user)
      case _@ ex: Throwable =>
        ex.printStackTrace()
        Future.successful(user)
    }
  }

  /**
   * Saves a user.
   * Ideally it should use UPSERT feature of the backend store but as of now it checks whether User exists or not and accordingly insert/update the record
   *
   * @param user The user to save.
   * @return The saved user.
   */
  def update(user: User): Future[User] = {
    println(s"Saving user: $user")
    val doc: Json = user.asJson
    try {
      find(user.userID).map(maybeUser => {
        maybeUser match {
          case Some(u) =>
            val _key = collection
              .updateDocument(user._key.getOrElse(""), printer.print(doc))
              .getKey()
            user.copy(_key = Some(_key))
          case _ =>
            add(user)
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
    Future.successful(user)
  }

  /**
   * Saves a user.
   * Ideally it should use UPSERT feature of the backend store but as of now it checks whether User exists or not and accordingly insert/update the record
   *
   * @param user The user to save.
   * @return The saved user.
   */
  def save(user: User): Future[User] = {
    import io.circe.optics.JsonPath._
    println(s"Saving user: $user")
    val doc: Json = user.asJson
    try {
      val insertDoc: String = printer.print(doc) //.replaceAll("""@""", """@@""")
      val updatedDoc: String = printer.print(doc) //.replaceAll("""@""", """@@""")
      val query =
        s"""UPSERT { userID: @userID } 
        INSERT $insertDoc
        REPLACE $updatedDoc IN $collectionName
        RETURN { doc: NEW, action: OLD ? "update" : "insert" }
        """
      /*
      val query =
        s"""UPSERT { userID: @userID }
        INSERT $insertDoc
        REPLACE $updatedDoc IN $collection
        RETURN { doc: NEW, type: OLD ? 'update' : 'insert' }
        """
       */
      println(s"Q for save: $query")
      val bindVars: java.util.Map[String, Object] =
        new com.arangodb.util.MapBuilder()
          .put("userID", user.userID)
          //.put("insertDoc", insertDoc)
          //.put("updatedDoc", updatedDoc)
          .get()
      val cursor: ArangoCursor[String] =
        db.db.query(query, bindVars, null, classOf[String])
      cursor
        .iterator()
        .asScala
        .map(c => {
          val parsed = parse(c).getOrElse(Json.Null)
          val docLens = root.doc.json.getOption(parsed)
          //val actionLens = root.action.string.getOption(parsed)
          println(s"doc lens: $docLens")
          //println(s"doc lens: $docLens and action lens: $actionLens")
          docLens.getOrElse(Json.Null).as[User] match {
            //decode[User](docLens) match {
            case Left(failure) =>
              println(s"Invalid JSON: $c")
            case Right(json) => println(s"Yay, got some JSON! $json")
          }
          decode[User](c)
        })
        .filter(x => x.isRight)
        .map(_.right.get)
        .toSeq
        .headOption
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
    Future.successful(user)
  }
}

object ArangoDbUserDAOImpl {

  /**
   * The list of users.
   */
  //val users: mutable.HashMap[UUID, User] = mutable.HashMap()
}
