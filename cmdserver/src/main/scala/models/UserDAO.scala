package models

import scala.collection.mutable

case class AppUser(username: String, userId: String, role: String)
/*
object UserDAO {
  val adminuser = System.getenv("APP_BASIC_AUTH_ADMIN_USER")
  val adminpwd = System.getenv("APP_BASIC_AUTH_ADMIN_PASSWORD")
  val user = System.getenv("APP_BASIC_AUTH_USER")
  val pwd = System.getenv("APP_BASIC_AUTH_PASSWORD")
  private val users = mutable.Map(
    adminuser -> AppUser(adminuser, adminpwd),
    user -> AppUser(user, pwd)
  )
  println(users)

  def getUser(username: String): Option[AppUser] = {
    users.get(username)
  }

  def addUser(username: String, password: String): Option[AppUser] = {
    if(users.contains(username)) {
      Option.empty
    } else {
      val user = AppUser(username, password)
      users.put(username, user)
      Option(user)
    }
  }

}*/
