package net.jtownson.xdfbinext

import com.opencsv.CSVReader
import net.jtownson.xdfbinext.CsvLogReader.{TransposedLog, isDataLine, isHeaderLine, isMetaComment, parseMetaComment}

import java.io.File
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import scala.util.Using
import scala.jdk.CollectionConverters.*

class CsvLogReader(val logfile: File) {

  val channels: Map[String, List[BigDecimal]] = Using.resource(new CSVReader(Files.newBufferedReader(logfile.toPath))) {
    reader =>
      val transposedLog = reader.readAll().asScala.foldLeft(TransposedLog()) { (log, line) =>
        if (isMetaComment(line))
          log.withMetadata(parseMetaComment(line))
        else if (isHeaderLine(line, log))
          log.withHeaderLine(line)
        else if (isDataLine(line, log))
          log.withDataLine(line)
        else
          log
      }
      transposedLog.asChannels
  }
}

object CsvLogReader {

  def isMetaComment(line: Array[String]): Boolean =
    line.length == 1 && line.head.replace("\uFEFF", "").startsWith("#")

  def isHeaderLine(line: Array[String], log: TransposedLog): Boolean =
    line.nonEmpty && log.headerLine.isEmpty

  def isDataLine(line: Array[String], log: TransposedLog): Boolean =
    log.headerLine.nonEmpty && line.nonEmpty

  def parseMetaComment(line: Array[String]): (String, String) = {
    val split = line.head.replace("\uFEFF", "").drop(1).split(':')
    require(split.length == 2, "Invalid metadata split in CSV header")
    (split(0).trim, split(1).trim)
  }

  case class TransposedLog(
      metadata: Map[String, String] = Map.empty,
      headerLine: Array[String] = Array.empty,
      dataLines: Array[ArrayBuffer[BigDecimal]] = Array.empty
  ) {
    def withMetadata(kv: (String, String)): TransposedLog =
      copy(metadata = metadata.updated(kv._1, kv._2))

    def withHeaderLine(line: Array[String]): TransposedLog =
      copy(headerLine = line, dataLines = Array.fill(line.length)(ArrayBuffer.empty[BigDecimal]))

    def withDataLine(line: Array[String]): TransposedLog = {
      require(line.length == headerLine.length, "Header/data size mismatch in CSV")
      val dataLine = line.map(BigDecimal(_))
      line.indices.foreach { i =>
        dataLines(i).append(dataLine(i))
      }

      this
    }

    def asChannels: Map[String, List[BigDecimal]] =
      headerLine.indices.map(i => headerLine(i) -> dataLines(i).toList).toMap

  }
}
