package api

import java.io.{File, FileInputStream, InputStream}

import com.monitorjbl.xlsx.StreamingReader
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.poifs.filesystem.POIFSFileSystem
import org.apache.poi.ss.usermodel.{Cell, Row, Workbook}
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import shared.{Employer, EmployerExtra}

import scala.collection.JavaConverters._
import com.microsoft.schemas.office.visio.x2012.main.CellType

object ExcelReader {
  def load(filename: String)= {
    val is: InputStream = new FileInputStream(new File(filename))
    val workbook: Workbook = StreamingReader.builder()
      .rowCacheSize(1000)    // number of rows to keep in memory (defaults to 10)
      .bufferSize(4096)     // buffer size to use when reading InputStream to file (defaults to 1024)
      .open(is)

    workbook.iterator().asScala.map(sheet => {
      val sheetName = sheet.getSheetName
      sheet.iterator().asScala.map(row => {
        row.iterator().asScala.map(cell => {

        })
      })
    })
  }

  //Using library from https://github.com/monitorjbl/excel-streaming-reader
  def loadEmployers(filename: String): List[Employer] = {
    val is: InputStream = new FileInputStream(new File(filename))
    val workbook: Workbook = new XSSFWorkbook(is)
    /*
    val workbook: Workbook = StreamingReader.builder()
      .rowCacheSize(1000)    // number of rows to keep in memory (defaults to 10)
      .bufferSize(4096)     // buffer size to use when reading InputStream to file (defaults to 1024)
      .open(is)
    */
    val records = workbook.iterator().asScala.map(sheet => {
      val sheetName = sheet.getSheetName
      val rowNo = sheet.getLastRowNum()
      println(s"Loading data from sheet = $sheetName with rows = $rowNo")
      //rowNo = 5
      val dataxs: scala.collection.mutable.ListBuffer[Employer] = scala.collection.mutable.ListBuffer.empty
      var rowno = 0
      while (rowno < rowNo) {
        if (rowno > 0) {
          val row: Row = sheet.getRow(rowno)
          if (row == null) {

          } else {
            val cells: List[String] = (0 to 30).toList.map(cno => {
              val cell: Cell = row.getCell(cno)
              if (cell == null) {
                ""
              } else {
                if (cell.getCellType() == org.apache.poi.ss.usermodel.CellType.NUMERIC) {
                  cell.getNumericCellValue().toString()
                } else cell.getStringCellValue
              }
            })
            val sz = cells.size
            val colNos = row.getPhysicalNumberOfCells()
            val name = cells(5).trim
            println(s"No of cols = $sz and physical no of cells = $colNos for name = $name")
            val department = if (cells(4).isEmpty) None else Some(cells(4).trim)
            val industry = if (cells(2).isEmpty) None else Some(cells(2).trim)
            val area = if (cells(6).isEmpty) None else Some(cells(6).trim)
            val website = cells(9).replaceAll("/$", "").trim.toLowerCase
            val domain = Employer.getWebsite(website)
            //No from where URL starts
            var i = 10
            val urls: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
            while(i < sz) {
              urls.append(cells(i).trim)
              i += 1
            }
            val extra = EmployerExtra(category = cells(0).trim, stateType = cells(1).trim, state = cells(3).trim, area = area.getOrElse(""), department = department, industry = industry, links = Some(urls.toList.filter(url => !url.trim.isEmpty)))
            val record = Employer(hash = Some(s"${name.toLowerCase} - $website"), name = name, code = cells(8).trim, website = website, domain = Some(domain), extra = extra)
            dataxs.append(record)
          }
        } else {
        }
        rowno += 1
      }
      val totals = dataxs.size
      println(s"Found total no of records = $totals")
      /*
      sheet.iterator().asScala.drop(1).take(2).map(row => {
        val cells = row.iterator().asScala.toList
        val sz = cells.size
        val colNos = row.getPhysicalNumberOfCells()
        val name = cells(5).getStringCellValue.trim
        println(s"No of cols = $sz and physical no of cells = $colNos for name = $name")
        val department = if (cells(4).getStringCellValue.isEmpty) None else Some(cells(4).getStringCellValue.trim)
        val industry = if (cells(2).getStringCellValue.isEmpty) None else Some(cells(2).getStringCellValue.trim)
        val area = if (cells(6).getStringCellValue.isEmpty) None else Some(cells(6).getStringCellValue.trim)
        val website = cells(9).getStringCellValue.replaceAll("/$", "").trim.toLowerCase
        var i = 10
        val urls: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
        while(i < sz) {
          urls.append(cells(i).getStringCellValue.trim)
        }
        val extra = EmployerExtra(category = cells(0).getStringCellValue.trim, stateType = cells(1).getStringCellValue.trim, state = cells(3).getStringCellValue.trim, area = area.getOrElse(""), department = department, industry = industry, links = Some(urls.toList))
        val record = Employer(hash = Some(s"${name.toLowerCase} - $website"), name = name, code = cells(8).getStringCellValue.trim, website = website, extra = extra)
        record
      })
       */
      dataxs.toList
    })
    val xs = records.flatten.toList
    //println(xs.take(100))
    println(s"Found no of lines in document = $filename == ${xs.size}")
    xs
  }

}
