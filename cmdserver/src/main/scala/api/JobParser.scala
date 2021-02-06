package api
import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.util.matching.Regex
import shared.{DatesInfo, EducationTag, Employer, FullJob, MatchingRule}

import scala.collection.JavaConverters._
object DateHelper {

  //Using lib: http://natty.joestelmach.com/
  def getAllDates(data: String): Seq[java.util.Date] = {
      try {
          import com.joestelmach.natty._

          val parser: com.joestelmach.natty.Parser = new com.joestelmach.natty.Parser()
          val groups: java.util.List[com.joestelmach.natty.DateGroup] = parser.parse(data)
          groups.asScala.map(group => {
              val dates: java.util.List[java.util.Date] = group.getDates()
              //println(s"Dates: $dates")
              val line = group.getLine()
              val column = group.getPosition()
              val matchingValue = group.getText()
              val syntaxTree = group.getSyntaxTree().toStringTree()
              val parseMap = group.getParseLocations()
              val isRecurreing = group.isRecurring()
              val recursUntil: java.util.Date = group.getRecursUntil()
              //println(s"Date: $recursUntil")
              dates.asScala.toSeq
          }).toSeq.flatten
      }
        catch {
            case e: Exception =>
            Seq.empty
        }
  }
}

class JobParser(job: FullJob, employers: Seq[Employer],  matchingRules: Seq[MatchingRule]) {
    val isParseDates = false
    val RULES = {
        val xs = matchingRules.filter(m => m.matchType.equalsIgnoreCase("Education") && !m.category.isEmpty).filter(m => !m.category.equalsIgnoreCase("Specialisation") && !m.category.equalsIgnoreCase("Level"))
        //xs.filter(m => m.name.equalsIgnoreCase("Agri"))
        xs
    }
    var foundEmployers: Seq[Employer] = Seq.empty
    val html: String = job.content.map(c => c.content.replace("\"", "").replace("\'", "")).getOrElse("")
    //val content: String = job.content.map(c => c.name + " " + c.content.replaceAll("""""""","")).getOrElse("")
    def replaceBadCharacters(s: String): String = {
        val tmp = s.replaceAll("“", "")
          .replaceAll("”", "")
          .replaceAll("‘", "")
          .replaceAll("’", "")
      tmp
    }
    val tagName = job.originalLink.getOrElse("")
    val doc: Document = Jsoup.parse(html)
    val originalContentText =  replaceBadCharacters(doc.head().wholeText() + " " + doc.body().wholeText()).replaceAll("""\s+""", " ")
    val content =  replaceBadCharacters(job.content.map(c => c.name).getOrElse("") + " " + doc.head().wholeText() + " " + doc.body().wholeText()).replaceAll("[^a-zA-Z0-9,\\.\\+\\-\\(\\)\\/\\&\\*\\s+]", "").replaceAll("""\s+""", " ")
    val data: String = content.toLowerCase
    val tokens: Seq[(String, Int)] = data.split("""\s""").map(_.trim).zipWithIndex
    //println(s"Tokens: $tokens")
    //app.utils.Utils.saveToFile("/tmp/content.txt", content, prefix = "", appendNewline = false)

    def checkStartDate(input: String) = {
        List("start date of receipt", "start date of", "start date", "from date", " dated").foldLeft(false)((a, b) => a || input.contains(b))
    }

    def checkLastDate(input: String) = {
        List("last date of submission",  "last date of", "last date", "on or before", "till date").foldLeft(false)((a, b) => a || input.contains(b))
    }

    val walkInMatch = List("walk-in", "walk-in -interview", "walk in","walking interview"," walking ", "walking-in")

    def checkStartDateOnPriority(input: String) = {
        List(" from ", " starting ").foldLeft(false)((a, b) => a || input.contains(b))
    }

    def checkLastDateOnPriority(input: String) = {
        List(" by ", " to ").foldLeft(false)((a, b) => a || input.contains(b))
    }

    def parseDatesFromText(text: String): Seq[EducationTag] = {
        try {
            val parser: net.rationalminds.Parser = new net.rationalminds.Parser()
            val dates: java.util.List[net.rationalminds.LocalDateModel] = parser.parse(text)
            val result = dates.asScala.toSeq
            //println(result)
            val datesList1: Seq[EducationTag] = result.map(dt => {
                val skipCharLen = 200
                val subtext = data.drop(dt.getEnd() - skipCharLen).take(skipCharLen + 1)
                val isStart = checkStartDate(subtext)
                val isLast = checkLastDate(subtext)
                val level = if (isLast) "Last Date" else if (isStart) "Start Date" else ""
                val date = dt.getDateTimeString().split(" ").headOption.getOrElse("")
                val dts = app.utils.Utils.parseDateFromText(date)
                //println(s"start = $isStart, last = $isLast for date: $date with parsed values: ${dts} and level = $level")
                dts.map(x => EducationTag(name = x.toString, mainTag = "Dates", category = "Dates", level = level, _key = None, priority = skipCharLen))
            }).flatten
            val datesList2: Seq[EducationTag] = result.map(dt => {
                val skipCharLen = 50
                val subtext = data.drop(dt.getEnd() - skipCharLen).take(skipCharLen + 1)
                val isStart = checkStartDateOnPriority(subtext)
                val isLast = checkLastDateOnPriority(subtext)
                val level = if (isLast) "Last Date" else if (isStart) "Start Date" else ""
                val date = dt.getDateTimeString().split(" ").headOption.getOrElse("")
                val dts = app.utils.Utils.parseDateFromText(date)
                //println(s"start = $isStart, last = $isLast for date: $date with parsed values: ${dts} and level = $level")
                dts.map(x => EducationTag(name = x.toString, mainTag = "Dates", category = "Dates", level = level, _key = None, priority = skipCharLen))
            }).flatten
            val datesList = datesList1 ++ datesList2
            datesList
        } catch {
            case e: Exception =>
                val error = app.utils.Utils.stackTrace(e)
                //println(s"Date parsing error = $error")
            Seq.empty
        }
    }

    def getPotentialTokens(input: String) = {
        tokens.map(s => {
            s._1.equals(input) match {
                case true =>
                    //println(s"Input: $input matches at pos: ${s._2}")
                    tokens.drop(s._2 - 150).take(150 + 1).map(_._1).mkString(" ")
                case false =>
                    ""
            }
        }).filter(s => !s.isEmpty)
    }

    def findAdvNo(): Seq[EducationTag] = {
      try {
        val advRegex = new Regex("""(Advertisement|Adv)?\s?No.?:?(.*)(Date|\n)""")
        val founds = advRegex.findAllMatchIn(content).toList
        val values = founds.map(s => s.subgroups).flatten
        //println(s"Adv values $values")
        values match {
            case head :: tail :: Nil =>
                Seq(EducationTag(name = head, mainTag = "Adv", category = "Adv", level = "", _key = None))
            case head :: Nil =>
                Seq(EducationTag(name = head, mainTag = "Adv", category = "Adv", level = "", _key = None))
            case _ =>
                Seq.empty
        }
    }catch {
        case e: Exception =>
            Seq.empty
    }

}
    val prefixUrlEmployer = job.source.url.getOrElse("")
      .replaceAll("""https://""", "")
      .replaceAll("""http://""", "")
       .replaceAll("""www""","")
      .split("/")
      .toList
      .headOption
      .getOrElse("").toLowerCase

    def findCompany(): Seq[EducationTag] = {
        try {
            val empMatches: Seq[Tuple2[Boolean, Employer]] = employers.map(emp => {
                if ((!emp.website.isEmpty) && emp.website.toLowerCase.contains(prefixUrlEmployer)) {
                        println(s"Found Employer by site url = ${job.source.url}")
                    (true, emp)
                } else {
                    val code = emp.code.toLowerCase
                    val site = emp.website.replaceAll("""https://""", "").replaceAll("""http://""","").replaceAll("""www""","").replaceAll("https","").replaceAll("""http""","").trim
                    val case3 = !site.isEmpty && data.contains(site.toLowerCase)
                    val case21 = !emp.name.isEmpty && data.contains(emp.name.toLowerCase) && emp.name.split(" ").size >= 2
                    val cleanedName = emp.name.replaceAll("""\&""", "and").replaceAll("""&""","and")
                    val case22 = !cleanedName.isEmpty && data.contains(cleanedName.toLowerCase) && cleanedName.split(" ").size >= 2
                    val case2 = case21 || case22
                    val case1 = !emp.code.isEmpty && data.contains(s" $code ", s"($code)", s" $code")
                    val match1 = case3 && case2
                    val match2 = case3 && case1
                    val match3 = case2 && case1
                    val match4 = case3
                    if (match1 || match2 || match3 ||match4) {
                        //println(s"Name: ${emp.name} case1 = $case1, case2 = $case2 and case3 = $case3")
                        //println(s"1: $match1, 2: $match2, 3: $match3 and 4: $match4")
                    }
                    (case3 || (case3 && case2) || (case3 && case1) || (case2 && case1), emp)
                }
        })
        //println(s"Employer matches: $empMatches")
        val filteredEmps = empMatches.filter(x => x._1)
        //println(s"Employer matches: $filteredEmps")
        foundEmployers = filteredEmps.map(_._2)
        filteredEmps.map(emp => EducationTag(name = emp._2.name, mainTag = "Employer", category = "Employer", level = "", _key = emp._2._key))
    }catch {
        case e: Exception =>
            Seq.empty
    }
    }
    def findPossibleDates(): Seq[EducationTag] = {
        try {
            val currentDate = ZonedDateTime.now()
            val afterDate = currentDate.minusYears(1L).toInstant.toEpochMilli
            val beforeDate = currentDate.plusYears(2L).toInstant.toEpochMilli
            //println(s"afterDate: $afterDate and beforeDate = $beforeDate")
            val dates2 = parseDatesFromText(content)
            val dates1 = {DateHelper.getAllDates(content)}.distinct.filter(d => d.getTime >= afterDate && d.getTime <= beforeDate).sorted.map(x => {
                //val tmpDate = LocalDate.parse(x.toString, java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd"))
                EducationTag(name = x.toString, mainTag = "Dates", category = "Dates", level = "", _key = None)
            }).filter(x => !x.name.isEmpty).distinct
            val possibleDates = dates1.map(dt => (dt, DatesInfo.toLocalDate(dt.name, ""))).filter(dt => dt._2.isDefined).distinct
            val result1: Seq[shared.EducationTag] = possibleDates.map(x => {
              val yearFromDate = x._2.get.getYear
                val dayFromDate = app.utils.Utils.leftPad(x._2.get.getDayOfMonth.toString, 2, '0')
              val matches: Seq[String] = getPotentialTokens(yearFromDate.toString)
                //println(s"Checking year = $yearFromDate and day = $dayFromDate")
              val isStart = matches.map(mat => {
                  val s = mat.replaceAll("[\\.\\/\\-]", " ").replaceAll("""\s+""", " ")
                  val part1 = checkStartDate(s)
                  val daystr = s.replaceAll("[^0-9\\s+]", "").replaceAll("""\s+""", " ").split(" ").toSeq.map(_.trim).filter(!_.isEmpty).toSeq
                  val part2 = daystr.foldLeft(false)((a, b) => a || b.equals(dayFromDate))
                  //println(s"Checking in $s part1 = $part1 and part2 = $part2 (day = $dayFromDate $daystr)")
                  part1 && part2
              }).distinct.find(s => s == true).headOption.getOrElse(false)
                val isLast = matches.map(mat => {
                    val s = mat.replaceAll("[\\.\\/\\-]", " ").replaceAll("""\s+""", " ")
                    val part1 = checkLastDate(s)
                    val daystr = s.replaceAll("[^0-9\\s+]", "").replaceAll("""\s+""", " ").split(" ").toSeq.map(_.trim).filter(!_.isEmpty).toSeq
                    val part2 = daystr.foldLeft(false)((a, b) => a || b.equals(dayFromDate))
                    //println(s"Checking in $s part1 = $part1 and part2 = $part2 (day = $dayFromDate $daystr)")
                    part1 && part2
                }).distinct.find(s => s == true).headOption.getOrElse(false)
              val level = if (isLast) "Last Date" else if (isStart) "Start Date" else ""
              //println(s"Year from date: $yearFromDate / day: $dayFromDate (of date: $x), matches: $matches with found: $isLast")
                x._1.copy(level = level)
            }).distinct
            val result2: Seq[EducationTag] = dates2
            val result = result2 ++ result1
            result.distinct
            //dates2.distinct
        } catch {
            case e: Exception =>
            e.printStackTrace()
            Seq.empty
        }
    }

    def findEmploymentType(): Seq[EducationTag] = {
      try {
        val case1 = List("permanent ", " permanent").foldLeft(false)((a, b) => a || data.contains(b))
        val case2 = List("contractual ", " contractual").foldLeft(false)((a, b) => a || data.contains(b))
        val case3 = List("apprenticeship ", " apprenticeship").foldLeft(false)((a, b) => a || data.contains(b))
        if (case1) Seq(EducationTag(name = "Permanent", mainTag = "Employement Type", category = "Employment", level = "")) else {
            if (case2) Seq(EducationTag(name = "Contractual", mainTag = "Employement Type", category = "Employment", level = "")) else {
                if (case3) Seq(EducationTag(name = "Apprenticeship", mainTag = "Employement Type", category = "Employment", level = "")) else {
                    Seq.empty
                }
            }
        }
    }catch {
        case e: Exception =>
            Seq.empty
    }

}

def findProcessType(): Seq[EducationTag] = {
        val case1 = walkInMatch.foldLeft(false)((a, b) => a || data.contains(b))
        if (case1) Seq(EducationTag(name = "Walk-In", mainTag = "Process Type", category = "Process", level = ""))else Seq.empty
    }
    //Approx 10 words
    val NO_OF_WORDS_FOR_BOUNDS = 10

    def isTagMatchedWithingBounds(tag: String, wordSizeApprox: Int = 15): Boolean = {
        val tags = tag.split("""\+""").toList.permutations.toList
        val charsTobeDropped = NO_OF_WORDS_FOR_BOUNDS * wordSizeApprox
        def stringMatchWithinBounds(input: String): Boolean = {
            if (input.isEmpty) false else {
                val pattern = "\\Q" + input + "\\E"
                val r = pattern.r
                val matches: List[Int] = r.findAllMatchIn(content).map(_.start).toList
                //println(s"Pattern: $r, indexes = $matches for $input (skip no: $charsTobeDropped")
                val case2 = matches.map(idx => {
                    val sub = content.drop(idx - charsTobeDropped).take(2 * charsTobeDropped)
                    //println(s"Index = $idx has sub = $sub for input = $input")
                    sub.contains(input)
                }).toSet.contains(true)
                case2
            }
        }

        val matches: List[Boolean] = tags.map(xs => {
            xs match {
                case first :: second :: third :: tail =>
                    val case1 = content.contains(first)
                    val case2 = stringMatchWithinBounds(second)
                    val case3 = stringMatchWithinBounds(third)
                    //println(s"isTagMatchedWithingBounds: first = $first, second = $second, third = $third has case1 = $case1, case2 = $case2 and case3 = $case3")
                    case1 && case2 && case3
                case first :: second :: tail =>
                    val case1 = content.contains(first)
                    val case2 = stringMatchWithinBounds(second)
                    //println(s"isTagMatchedWithingBounds: first = $first, second = $second, has case1 = $case1, case2 = $case2 ")
                    case1 && case2
                case first :: Nil =>
                    val case1 = content.contains(first)
                    //println(s"isTagMatchedWithingBounds: first = $first, has case1 = $case1")
                    case1
                case _ =>
                    false
            }
        })
      matches.toSet.contains(true)
    }

    def findEducationMatches(): Seq[EducationTag] = {
        try {
            RULES.map(rxs => {
                val matches: List[Boolean] = rxs.matches.map(m => {
                    val case1 = content.contains(m)
                    val case2 = if (m.contains("+")) isTagMatchedWithingBounds(m) else false
                    //println(s"findEducationMatches: M = $m case1 = $case1 and case = $case2")
                    case1 || case2
                })
                val found = matches.toSet.contains(true)
                if (found) Seq(EducationTag(name = rxs.name, mainTag = rxs.matchType, category = rxs.category, level = rxs.level.getOrElse(""))) else Seq.empty
            }).flatten
        }catch {
            case e: Exception =>
            Seq.empty
        }

    }

    val functions: List[Function0[Seq[EducationTag]]] = {
        if (isParseDates)
        List(
            findCompany, findAdvNo, findProcessType, findEmploymentType, findPossibleDates, findEducationMatches
        )
        else
            List(
                findCompany, findAdvNo, findProcessType, findEmploymentType, findEducationMatches
            )
    }

    def parse(): Tuple2[Seq[EducationTag], Seq[Employer]] = {
      try {
        val results: Seq[EducationTag] = functions.map(fn => {
            val result: Seq[EducationTag] = fn()
            result
        }).flatten ++ Seq(EducationTag(name = content, mainTag = "Content", category = "Content", level = ""))
        //println(s"JobParser: $results")
          (results, foundEmployers)
    } catch {
        case e: Exception =>
            e.printStackTrace()
            (Seq.empty, Seq.empty)
      }
    }
}

object JobParser {
    val EDUCATION_MAP:Map[String, List[String]] = {
        val bscMatch = List("/B.SC./", " B.SC. ", " B.SC ", "B.SC", "/B.Sc./", " B.Sc. ", " B.Sc ", "B.Sc")
        val mscMatch = List("/M.SC./", " M.SC. ", " M.SC ", "M.SC", "/M.Sc./", " M.Sc. ", " M.Sc ", "M.Sc")
        val phdMatch = List(" P.H.D. ", " PH.D. ", " PH.D ", " P H D", " PH D", "/Ph.D/", "/PH.d/")
        val btechMatch = List(" B.Tech ", " B.TECH ", "/B.Tech/", " B. Tech ", " B. TECH ", " BTECH ")
        val mtechMatch = List(" M.Tech ", " M.TECH ", "/M.Tech/", " M. Tech ", " M. TECH ", " MTECH ")
        val mbbsMatch = {
            List(" MBBS ", "M.B.B.S.", "M B B S", "M. B. B. S.")
        }
        val bdsMatch = {
            List("BUMS", "B.U.M.S.", "B U M S", "B. U. M. S."," BAMS ", "BAMS ", "B.A.M.S.", "B A M S", "B. A. M. S.",  "BHMS", "B H M S", "B.H.M.S.", "B. H. M. S.",  " BDS ", "B D. S.", "B D S", "/BDS/", " BDS.")
        }
        val mdMatch = {
            List(" M D ", "/MD/", "M.D.", "M. D."," MD ")
        }
        val dmMatch = {
            List("/D.M/", "/D M /", "D.M.", "D. M.")
        }
        val mchMatch = {
            List(" M.CH. ", " M CH", " M. CH.", " M Ch ", " M Ch.", " MCH.")
        }
        val msMatch = {
            List(" M.S. ", "/M S/", " M. S. ", "/MS/", " MS. ")
        }
        val paramedicalMatch = {
            List("B. PHARMA", " BPHARMA ", " B PHARMA ", " M. PHARMA ", " M PHARMA ", " MPHARMA ", "/MPHARMA/"," ANM ", "/ANM/", " GNM ", " DMLT ")
        }

        val mbaMatch = {
            List(" MBA ", " M.B.A ", " M B A ", " M.B.A. ", " M. B. A. "," PGDM ", "P.G.D.M")
        }
        val bbaMatch = {
            List("B B A", "B.B.A.", "B. B. A.","BCOM.", "B.COM.","BCOM."," BCOM ", "/BCOM/", " BCOM", "BCom.", "B.Com.","BCom."," BCom ", "/BCom/", " BCom")
        }
        val bedMatch = {
            List("B. ED.", "B.ED", "B.P.ED.", "B. Ed.", " B.Ed ", "/B.Ed/", " B.Ed", "B.P.Ed.")
        }
        val medMatch = {
            List("M.P.ED.", "MPED", "M. ED.", "M.ED", "M ED", " MED ", "/MED/", "MED.", "M.P.Ed.", "MPEd", "M. Ed.", "M.Ed", "M Ed", " MEd ", "/MEd/", "MEd.")
        }
        Map("BSc" -> bscMatch, "MSc" -> mscMatch, "BTech" -> btechMatch, "MTech" -> mtechMatch, "MBBS" -> mbbsMatch, "BDS" -> bdsMatch,
        "MD" -> mdMatch, "DM" -> dmMatch, "MCh" -> mchMatch, "MS" -> msMatch, "Pharma" -> paramedicalMatch,
        "MBA" -> mbaMatch, "BBA" -> bbaMatch, "BEd" -> bedMatch, "MEd" -> medMatch)
    }

    val CATEGORY_MAP = Map(
        "SC" ->  "SC",
        "SC " ->  "SC",
        "SC/" ->  "SC",
        "Scheduled Caste" ->  "SC",
        "Scheduled Caste/" ->  "SC",
        "SC/ST" ->  "SC/ST",
        "SC / ST" ->  "SC/ST",
        "OBC" ->  "OBC",
        "OBC/" ->  "OBC",
        "ST" ->  "ST",
        "ST " ->  "ST",
        "ST/" ->  "ST",
        "(ST)" ->  "ST",
        "PH/" ->  "PH",
        "PH" ->  "PH",
        "PH " ->  "PH",
        "Physically Handicap" ->  "PH",
        "GEN" ->  "General",
        "(GEN)" ->  "General",
        "GEN/" ->  "General",
        "GEN " ->  "General",
        "GENERAL/" ->  "General",
        "General/" ->  "General",
        "General " ->  "General",
        "General" ->  "General"
    )

    val STATE_CODE_AND_NAMES = List(
        ("Central Government", "Central Government"),
        ("Andaman and Nicobar Island", "Andaman and Nicobar Island (UT)"),
        ("Andhra Pradesh", "Andhra Pradesh"),
        ("Arunachal Pradesh", "Arunachal Pradesh"),
        ("Assam", "Assam"),
        ("Bihar", "Bihar"),
        ("Chandigarh (UT)", "Chandigarh"),
        ("Chandigarh", "Chandigarh (UT)"),
        ("Chhattisgarh", "Chhattisgarh"),
        ("Dadra and Nagar Haveli", "Dadra and Nagar Haveli (UT)"),
        ("Dadra and Nagar Haveli (UT)", "Dadra and Nagar Haveli"),
        ("Daman and Diu", "Daman and Diu (UT)"),
        ("Daman and Diu (UT)", "Daman and Diu"),
        ("Delhi", "New Delhi"),
        ("Delhi (NCT)", "Delhi (NCT)"),
        ("Delhi (NCR)", "Delhi (NCT)"),
        ("New Delhi", "New Delhi"),
        ("Goa", "Goa"),
        ("Gujarat", "Gujarat"),
        ("Haryana", "Haryana"),
        ("Himachal Pradesh", "Himachal Pradesh"),
        ("Jammu & Kashmir", "Jammu and Kashmir"),
        ("Jammu and Kashmir", "Jammu and Kashmir"),
        ("Jammu & Kashmir (J & K)", "Jammu and Kashmir"),
        ("Jharkhand", "Jharkhand"),
        ("Karnataka", "Karnataka"),
        ("Kerala", "Kerala"),
        ("Lakshadweep (UT)", "Lakshadweep"),
        ("Lakshadweep", "Lakshadweep"),
        ("Madhya Pradesh", "Madhya Pradesh"),
        ("Maharashtra", "Maharashtra"),
        ("Manipur", "Manipur"),
        ("Meghalaya", "Meghalaya"),
        ("Mizoram", "Mizoram"),
        ("Nagaland", "Nagaland"),
        ("Odisha", "Odisha"),
        ("Puducherry", "Puducherry"),
        ("Puducherry (UT)", "Puducherry"),
        ("Punjab", "Punjab"),
        ("Rajasthan", "Rajasthan"),
        ("Sikkim", "Sikkim"),
        ("Tamil Nadu", "Tamil Nadu"),
        ("Telangana", "Telangana"),
        ("Tripura", "Tripura"),
        ("Uttar Pradesh", "Uttar Pradesh"),
        ("Uttarakhand", "Uttarakhand"),
        ("West Bengal", "West Bengal")
    )
    val STATE_NAMES = STATE_CODE_AND_NAMES.map(_._2).toSet.toList.sortWith((a, b) => a < b)
    val CENTRE_WITH_STATE_NAMES = List("Center") ++ STATE_NAMES

    val EDUCATION_8_CLASS = List(
        "8th class",
        "8th pass",
        "8th passed",
        "VIII pass",
        "VIII passed ",
        "VIII class",
        "Class VIII ",
        "8 th pass",
        "8 th passed",
        "8 th class",
        "Passed VIII",
        "Passed 8th ",
        "Passed 8 th"
    )

    val EDUCATION_10_CLASS_MATCHES = List(
        "Matriculation",
        "10th class",
        "10th pass",
        "10th passed",
        "X pass",
        "X passed ",
        "X class",
        "Class X ",
        "10 th pass",
        "10 th passed",
        "10 th class",
        "Passed X",
        "Passed 10th ",
        "Passed 10 th",
        "Matric pass",
        "Metric Pass",
        "Matric passed",
        "Matric Pass",
        "SSC",
        "Senior Secondary Certificate",
        "Senior Secondary Pass",
        "Senior Secondary Passed",
        "Senior Secondary Exam",
        "passed SSLC",
        "SSLC",
        "passed S.S.L.C",
        "S.S.L.C",
        "CI 10th",
        "10th pass",
        "10th \"pass\"",
        "10th \"passed\""
    )

    val EDUCATION_12_CLASS_MATCHES = List(
        "Intermediate Exam",
        "Intermediate pass",
        "Intermediate passed",
        "Passed Intermediate",
        "12th class",
        "12th pass",
        "12th passed",
        "XII pass",
        "XII passed ",
        "XII class",
        "Class XII ",
        "12 th pass",
        "12 th passed",
        "12 th class",
        "Passed XII",
        "Passed 12th ",
        "Passed 12 th",
        "HSC",
        "Higher Secondary Certificate",
        "Higher Secondary Pass",
        "Higher Secondary Passed",
        "Higher Secondary Exam",
        "10+2",
        "10 + 2",
        "10th+2",
        "10th + 2"
    )

    val SPECIALIZATION = List("Medical", "Pharma", "Paramedical", "Engineering", "Agriculture", "Law", "Arts", "Hospitality", "Education / Teaching", "Management", "Accounts and Finance", "Science", "Design", "Architecture", "Mass Communication", "Computer Applications", "Aviation").sortWith((a, b) => a < b) ++ List("Any Graduate","Any PostGraduate", "Others")
    val LEVELS = List("Vocational", "Diploma", "Graduate", "PostGraduate")

}
