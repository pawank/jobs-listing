package app.utils

import java.io.File

import ammonite.ops._

object FileUtils {
  def getFiles(folder: String): Seq[Path] = {
    ls ! Path(folder)
  }

  def getFilenames(folder: String): Seq[String] = getFiles(folder).map(_.toString())

  def getFileSize(filename: String): Double = {
    val f = new File(filename)
    if (f.exists()) {
      f.length().toDouble / 1024
    } else 0
  }

  def getFileInKiloBytes(filename: String): String = {
    val value = getFileSize(filename)
    val v = f"$value%1.2f"
    s"${v} KB"
  }
}
