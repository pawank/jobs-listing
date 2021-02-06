package utils

import ammonite.ops._
import ammonite.ops.ImplicitWd._
import org.apache.commons.io.FileUtils

object ShellUtils {
  val isDebug = false

  def changeHttpHttps(url: String): String = {
    if (url.contains("https:")) url.replaceAll("""https://""", "http://") else url.replaceAll("""http://""", """https://""")
  }

  def fileDownloader(url: String, filename: String, connectTimeout: Int, readTimeout: Int, allowRedirect: Boolean, counter: Int = 0):String = {
    import sys.process._
    import java.net.URL
    import java.io.File
    val maxConnectTimeout = 60
    val maxReadTimeout = 300
    //new URL(url) #> new File(filename) !!
      //FileUtils.copyURLToFile(new URL(url), new File(filename))
    val cmd: String = if (allowRedirect) {
        s"""curl --connect-timeout $maxConnectTimeout --max-time $maxReadTimeout -L -o $filename "$url""""
      } else {
        if (url.startsWith("https:")) {
          s"""curl --insecure --connect-timeout $maxConnectTimeout --max-time $maxReadTimeout -IL -o $filename "$url""""
        } else {
          s"""curl --connect-timeout $maxConnectTimeout --max-time $maxReadTimeout -IL -o $filename "$url""""
        }
      }
    println(s"Running curl cmd = $cmd")
    val output: String = {cmd !!}
    if (output.isEmpty && (counter <= 3)) {
      val fileSize = app.utils.FileUtils.getFileSize(filename)
      //Less than 8KB
      if (fileSize <= 8) {
        val redirect = if (counter >= 2) !allowRedirect else allowRedirect
        fileDownloader(changeHttpHttps(url), filename, maxConnectTimeout, maxReadTimeout, redirect, counter + 1)
      } else {
        output
      }
    } else output
  }

  def runCommandViaProcess(cmd: String, args: Seq[String], debug: Boolean = false): (Int, String) = {
    import scala.sys.process._
    if (isDebug || debug) {
      val output = Seq(cmd) ++ args
      println(s"runCommandViaProcess: $output")
    }
    val output: String = {Seq(cmd) ++ args}.!!
    if (isDebug || debug) {
      println(s"runCommandViaProcess: $cmd $args == $output")
    }
    (0, output)
  }

  def runCommand(cmd: String, args: Seq[String]): CommandResult = {
    args match {
      case head:: tail :: Nil =>
        %%(cmd, head, tail)
      case head:: Nil =>
        %%(cmd, head)
      case _ =>
        %%(cmd, args.mkString(" "))
    }
  }

  def executeViaProcessBuilder(cmd: String, args: Seq[String], debug: Boolean = false, counter: Int = 0): Either[String, String] = {
    if (isDebug || debug) {
      println(s"ShellUtils: $cmd $args ")
    }


      val r = runCommandViaProcess(cmd, args, debug)
      r._1 match {
        case 0 =>
          if (r._2.contains("java.lang.RuntimeException: Nonzero exit value")) {
            if (counter <= 0) {
              val updatedArgs: Seq[String] = Seq(changeHttpHttps(args(0))) ++ args.tail
              executeViaProcessBuilder(cmd, updatedArgs, debug, counter + 1)
            } else {
              Right(r._2)
            }
          } else Right(r._2)
        case _ =>
          if (counter <= 0) {
            val updatedArgs: Seq[String] = Seq(changeHttpHttps(args(0))) ++ args.tail
            executeViaProcessBuilder(cmd, updatedArgs, debug, counter + 1)
          } else {
            Left(r._2)
          }
      }
  }

  def execute(cmd: String, args: Seq[String]): Either[String, String] = {
      try {
        val cr = runCommand(cmd, args)
        if (isDebug)
          println(s"ShellUtils execute: $cr")
        cr.exitCode match {
          case 0 =>
            Right(cr.toString())
          case _ =>
            Left(cr.err.string)
        }
      } catch {
        case ex: ammonite.ops.ShelloutException =>
          if (isDebug)
            ex.printStackTrace()
          val error = app.utils.Utils.stackTrace(ex)
          Left(error)
        case _@ex: Throwable =>
          if (isDebug)
            println(ex)
          Left(app.utils.Utils.stackTrace(ex))
      }
    }
}
