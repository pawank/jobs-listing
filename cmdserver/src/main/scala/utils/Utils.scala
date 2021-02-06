package app.utils

import java.io.{File, FileOutputStream, FileWriter, IOException, InputStream, OutputStream}
import java.lang.management.{ManagementFactory, OperatingSystemMXBean}
import java.net.{InetSocketAddress, Socket, SocketAddress, SocketTimeoutException}
import java.time.{LocalDate, ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit
import java.util.{Date, Scanner, TimeZone, UUID}

import awscala.DateTime

import scala.language.implicitConversions

object Utils {
  val FILE_SEPARATOR = System.getProperty("file.separator")
  val TMP_DIR = System.getProperty("java.io.tmpdir")

  def getUniqueID(): String = {
    java.util.UUID.randomUUID().toString.replaceAll("-","")
  }

  /**
   * Get timezone tuple with (full formated timezone with GMT, just timezone ID)
   * @param tz Timezone
   * @return tuple of GMT based time zone and just timezone ID
   */
  def displayTimeZone(tz: TimeZone): (String, String) = {
    val hours: Long = TimeUnit.MILLISECONDS.toHours(tz.getRawOffset().toLong)
    val minutesTmp: Long = TimeUnit.MILLISECONDS.toMinutes(tz.getRawOffset().toLong) - TimeUnit.HOURS.toMinutes(hours)
    // avoid -4:-30 issue
    val minutes: Long = scala.math.abs(minutesTmp)
    if (hours > 0) {
      ("(GMT+%d:%02d) %s".format(hours, minutes, tz.getID()), tz.getID)
    } else {
      ("(GMT%d:%02d) %s".format(hours, minutes, tz.getID()), tz.getID)
    }
  }

  /**
   * Get a list of all time zones
   * e.g. (GMT-12:00) Etc/GMT+12, (GMT-11:00) Etc/GMT+11, (GMT-11:00) Pacific/Midway, (GMT-11:00) Pacific/Niue etc
   * @return
   */
  def getAllTimeZonesList: List[(String, String)] = {
    val ids: Array[String] = TimeZone.getAvailableIDs
    ids.toList.map(id => displayTimeZone(TimeZone.getTimeZone(id)))
  }
  def camelToDisplay(name: String, isCapitalise: Boolean = true) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    " " + { if (isCapitalise) m.group(0).capitalize else m.group(0).toLowerCase }
  }).capitalize

  /**
   * Takes a camel cased identifier name and returns a dot version of the same
   *
   * Example:
   *     camelToDot("startDate") == "start.date"
   * @param name Input to be made displayable
   */
  def camelToDot(name: String) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    "." + { m.group(0).toLowerCase }
  })

  val yyyyMMDDFormat = new java.text.SimpleDateFormat("dd/MM/yyyy")
  def optDate(str: String): Option[Date] = if (str.size > 0) Some(yyyyMMDDFormat.parse(str)) else None

  val dateFormatYYYYMMDD = "dd/MM/yyyy"
  val dateFormatUTC = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z"

  def asDisplayDate(date: LocalDate): String = {
    val pat = DateTimeFormatter.ofPattern("EEE, d MMM yyyy")
    date.format(pat)
  }

  def toDisplayDate(date: ZonedDateTime): String = {
    val pat = DateTimeFormatter.ofPattern("EEE, d MMM yyyy")
    date.format(pat)
  }

  def parseDateFromText(str: String): Seq[Date] = {
    val formats = Seq("dd/MM/yyyy", "dd-MM-yyyy", "dd.MM.yyyy", "yyyy.MM.dd", "yyyy-MM-dd", "yyyy/MM/dd")
    if (str.size > 0) formats.map(f => {
      try {
        Some(new java.text.SimpleDateFormat(f).parse(str))
      } catch {
        case e: Exception =>
          None
      }
    }).filter(_.isDefined).map(_.get) else Seq.empty
  }

  def leftPad(str:String,
              paddedLength:Int,
              ch:Char ='.' ) : String = {
    var remLength = paddedLength - str.length
    if (remLength <= 0) {
      str
    } else {
      val builder = StringBuilder.newBuilder
      for(a <- 0 until remLength){
        builder.append(ch)
      }
      builder.append(str)
      builder.toString()
    }
  }

  def saveToFile(filename: String, input: Array[Byte]): Option[String] = {
    val stream: FileOutputStream = new FileOutputStream(filename)
    try {
      stream.write(input)
    } finally {
      stream.close()
    }
    println(s"Wrote filename - $filename")
    Some(filename)
  }

  /**
   * This method ensures that the output String has only
   * valid XML unicode characters as specified by the
   * XML 1.0 standard. For reference, please see
   * <a href="http://www.w3.org/TR/2000/REC-xml-20001006#NT-Char">the
   * standard</a>. This method will return an empty
   * String if the input is null or empty.
   *
   * @param in The String whose non-valid characters we want to remove.
   * @return The in String, stripped of non-valid characters.
   */
  def stripNonValidXMLCharacters(in: String): String = {
    val out: StringBuffer = new StringBuffer
    var current: Char = 0
    if (in == null || (("" == in))) {
      ""
    } else {
      var i: Int = 0
      while (i < in.length) {
        {
          current = in.charAt(i)
          if ((current == 0x9) || (current == 0xA) || (current == 0xD) || ((current >= 0x20) && (current <= 0xD7FF)) || ((current >= 0xE000) && (current <= 0xFFFD)) || ((current >= 0x10000) && (current <= 0x10FFFF))) out.append(current)
        }
        ({
          i += 1; i - 1
        })
      }
    }
    out.toString
  }

  def saveToFile(filename: String, input: String, prefix: String = s"${TMP_DIR}${FILE_SEPARATOR}_portal_", appendNewline: Boolean = false): Option[String] = {
    val finalFilename = if (filename.contains(s"${TMP_DIR}${FILE_SEPARATOR}")) filename else { prefix + filename }
    val p = new java.io.PrintWriter(finalFilename)
    try {
      val inputdata = if (appendNewline) { input + "\n" } else input
      p.write(inputdata)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    } finally {
      p.close()
    }
    println(s"Saved file at $finalFilename")
    Some(finalFilename)
  }


  def writeLog(filename: String, input: String, appendNewline: Boolean = false): Option[String] = {
    val fw = new FileWriter(filename, true)
    try {
      fw.write(if (appendNewline) { input + "\n" } else input)
    } finally fw.close()
    Some(filename)
  }

  def getFileContent(filename: String): Option[String] = {
    try {
      if (new java.io.File(filename).exists()) {
        Some(scala.io.Source.fromFile(filename).mkString)
      } else None
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }
  def deleteFile(filename: String): Boolean = {
    try {
      new File(filename).delete()
      true
    } catch {
      case e: Exception =>
        e.printStackTrace()
        false
    }
  }

  def equalsOptions[T](opt1: Option[T], opt2: Option[T]): Boolean = PartialFunction.cond((opt1, opt2)) { case (Some(x), Some(y)) => x == y }

  def equalsStringOptions(opt1: Option[String], opt2: Option[String]): Boolean = PartialFunction.cond((opt1, opt2)) { case (Some(x), Some(y)) => x.equals(y) }

  def optionValueOrEmpty(opt: Option[String]): String = opt.getOrElse("")

  def md5(text: String): String = {
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("MD5")
    digest.digest(text.getBytes).map("%02x".format(_)).mkString
  }

  implicit def stackTrace(exp: Throwable): String = {
    import java.io.PrintWriter
    import java.io.StringWriter

    val sw: StringWriter = new StringWriter();
    val pw: PrintWriter = new PrintWriter(sw)
    exp.printStackTrace(pw)
    sw.toString()
  }

  def encodeUrl(data: String): String = {
    import java.nio.charset.{ StandardCharsets => SC }
    import play.utils.UriEncoding
    UriEncoding.encodePathSegment(data, SC.US_ASCII.name)
  }

  def decodeUrl(data: String): String = {
    import java.nio.charset.{ StandardCharsets => SC }
    import play.utils.UriEncoding
    UriEncoding.decodePath(data, SC.US_ASCII.name)
  }

  def getSimpleUID = UUID.randomUUID().toString.take(8)
  def encodeQueryString(input: String): String = {
    import java.net.{ URLDecoder, URLEncoder }
    URLEncoder.encode(input, "UTF-8")
  }

  def decodeQueryString(input: String): String = {
    import java.net.{ URLDecoder, URLEncoder }
    URLDecoder.decode(input, "UTF-8")
  }

  /**
   * isAlive Utility
   *
   * @param hostName
   * @param port
   * @return boolean - true/false
   */
  def isSocketAlive(hostName: String, port: Int): Boolean = {
    //timeout in ms
    val timeout: Int = 2000
    // Creates a socket address from a hostname and a port number
    val socketAddress: SocketAddress = new InetSocketAddress(hostName, port)
    val socket: Socket = new Socket()
    try {
      socket.connect(socketAddress, timeout)
      socket.close()
      true;
    } catch {
      case e: SocketTimeoutException =>
        System.out.println("SocketTimeoutException " + hostName + ":" + port + ". " + e.getMessage())
        false
      case e: IOException =>
        System.out.println(
          "IOException - Unable to connect to " + hostName + ":" + port + ". " + e.getMessage())
        false
      case e: Exception =>
        false
    }
  }
  def isPortOpen(port: Int): Boolean = {
    import java.io.IOException
    import java.net.DatagramSocket
    import java.net.ServerSocket
    var ss: ServerSocket = null
    var ds: DatagramSocket = null
    var status = false
    try {
      ss = new ServerSocket(port)
      ss.setReuseAddress(true)
      ds = new DatagramSocket(port)
      ds.setReuseAddress(true)
      status = true
    } catch {
      case e: IOException =>
        e.printStackTrace()
        val err = stackTrace(e)
        if (err.contains("java.net.BindException: Address already in use")) {
          status = true
        }
    } finally {
      if (ds != null) ds.close()
      if (ss != null) try
        ss.close()
      catch {
        case e: IOException =>
        /* should not be thrown */
      }
    }
    status
  }

  def getSystemInfo() = {
    val operatingSystemMXBean: OperatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean()
    val cpu = operatingSystemMXBean.getSystemLoadAverage() / operatingSystemMXBean.getAvailableProcessors()
    scala.math.round(cpu * 100)
  }

  def uploadToS3(file: String, key: String): Either[String, String] = {
    import awscala.Region
    import awscala.s3._
    try {
      val bucketName = System.getenv("OUTPUT_BUCKET")
      val AWS_ACCESS_KEY_ID = {
        System.getenv("MY_AWS_ACCESS_KEY_ID")
      }
      val AWS_SECRET_ACCESS_KEY = {
        System.getenv("MY_AWS_SECRET_ACCESS_KEY")
      }
      implicit val region = Region.Mumbai
      implicit val s3     = S3(AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY)

      val maybeBucket = s3.buckets.filter(s => s.name.equalsIgnoreCase(bucketName)).headOption
      val bucket: Bucket = if (maybeBucket.isDefined) {
        maybeBucket.get
      } else {
        s3.createBucket(bucketName)
      }
      val r: PutObjectResult = bucket.putAsPublicRead(key, new java.io.File(file))
      val s3obj: Option[S3Object] = bucket.getObject(key)
      val publicUrl: Option[String] = s3obj.map { obj =>
        //obj.publicUrl
        val url = obj.publicUrl
        //val url = obj.generatePresignedUrl(DateTime.now.plusDays(7)) // ?Expires=....
        //bucket.delete(obj) // or obj.destroy()
        url.toString
      }
      if (publicUrl.isDefined) Right(publicUrl.getOrElse("")) else Left(s"URL cannot be set to public, $file")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Left(app.utils.Utils.stackTrace(e))
    }
  }

  def writeToFile(bytes: Array[Byte], targetFile: String): String =
    try {
      val os: OutputStream = new FileOutputStream(targetFile)
      os.write(bytes)
      os.close()
      targetFile
    } catch {
      case e: Exception =>
        e.printStackTrace()
        ""
    }
  
 def getZonedDateTime(date: String) = {
    val DATE_TIME_FORMATTER: DateTimeFormatter  = DateTimeFormatter
      .ofPattern("yyyy-MM-dd")
    val ldate = LocalDate.parse(date)
    val ldatetime = ldate.atTime(0,0,0)
    ldatetime.atZone(ZoneId.of("UTC"))
    //val zdtInstanceAtOffset: ZonedDateTime = ZonedDateTime.parse(date, DATE_TIME_FORMATTER)
    //zdtInstanceAtOffset.withZoneSameInstant(ZoneOffset.UTC)
  }

  def localToZonedDateInUTC(localDate: LocalDate): ZonedDateTime = {
    val ldatetime = localDate.atTime(0,0,0)
    ldatetime.atZone(ZoneId.of("UTC"))
  }

  def getCurrentZonedDateInUTC(): ZonedDateTime = ZonedDateTime.now(ZoneId.of("UTC"))
  
  def isExpired(jobExpiry: ZonedDateTime, wrtDate: ZonedDateTime, expiryDays: Long = 15L) = {
    val expiryNDaysDate: ZonedDateTime = wrtDate.minusDays(expiryDays)
    jobExpiry.toInstant().toEpochMilli() < expiryNDaysDate.toInstant().toEpochMilli()
  }

}
