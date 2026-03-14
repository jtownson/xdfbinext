package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.{XdfModel, isNotUndefinedAddress}
import scopt.{OParser, OParserBuilder}

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.Using

object XdfAddressCompare {

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val leftModel  = parseXdf(config.leftXdf)
        val rightModel = parseXdf(config.rightXdf)

        val rows = joinByName(leftModel, rightModel)
        val csv  = toNameJoinCsv(rows)
        Files.writeString(config.output.toPath, csv, StandardCharsets.UTF_8)

      case _ =>
    }
  }

  private def parseXdf(file: File): XdfModel = {
    Using.resource(Source.fromFile(file))(xdfResource => XdfParser.parse(xdfResource.mkString))
  }

  def joinByAddress(left: XdfModel, right: XdfModel): Seq[AddressJoinRow] = {
    val leftByAddress  = left.tableAxisAddresses().groupBy(_._2)
    val rightByAddress = right.tableAxisAddresses().groupBy(_._2)
    val allAddresses   = (leftByAddress.keySet ++ rightByAddress.keySet).toSeq.sorted

    allAddresses.map { address =>
      AddressJoinRow(
        address = address,
        leftEntries = leftByAddress.getOrElse(address, Seq.empty),
        rightEntries = rightByAddress.getOrElse(address, Seq.empty)
      )
    }
  }

  def joinByName(left: XdfModel, right: XdfModel): Seq[NameJoinRow] = {
    val leftByName  = tableMainAddresses(left)
    val rightByName = tableMainAddresses(right)
    val allNames    = (leftByName.keySet ++ rightByName.keySet).toSeq.sorted

    allNames.map { name =>
      NameJoinRow(
        tableName = name,
        leftAddress = leftByName.get(name),
        rightAddress = rightByName.get(name)
      )
    }
  }

  private def tableMainAddresses(model: XdfModel): Map[String, Long] = {
    model.tables
      .map(table => table.title -> table.axes.z.embeddedData.mmedAddress)
      .filter((_, address) => isNotUndefinedAddress(address))
      .toMap
  }

  private def toNameJoinCsv(rows: Seq[NameJoinRow]): String = {
    val header = Seq(
      "table_name",
      "left_address",
      "right_address",
      "address_diff"
    )
    val data = rows.map { row =>
      Seq(
        row.tableName,
        formatAddress(row.leftAddress),
        formatAddress(row.rightAddress),
        formatSignedMagnitudeAddress(row.addressDiff)
      )
    }

    (header +: data).map(toCsvLine).mkString("", "\n", "\n")
  }

  private def formatAddress(address: Option[Long]): String =
    address.map(a => s"0x${a.toHexString.toUpperCase}").getOrElse("")

  private def formatSignedMagnitudeAddress(address: Option[Long]): String =
    address
      .map { a =>
        val sign      = if (a < 0) "-" else ""
        val magnitude = BigInt(a).abs
        s"${sign}0x${magnitude.toString(16).toUpperCase}"
      }
      .getOrElse("")

  private def toCsvLine(cells: Seq[String]): String =
    cells.map(escapeCsv).mkString(",")

  private def escapeCsv(cell: String): String = {
    val escaped = cell.replace("\"", "\"\"")
    s""""$escaped""""
  }

  case class AddressJoinRow(
      address: Long,
      leftEntries: Seq[(String, Long)],
      rightEntries: Seq[(String, Long)]
  ) {
    val matchType: String = {
      val leftEmpty  = leftEntries.isEmpty
      val rightEmpty = rightEntries.isEmpty
      if (!leftEmpty && !rightEmpty) "both"
      else if (!leftEmpty) "left_only"
      else "right_only"
    }
  }

  case class NameJoinRow(tableName: String, leftAddress: Option[Long], rightAddress: Option[Long]) {
    val addressDiff: Option[Long] = for {
      l <- leftAddress
      r <- rightAddress
    } yield r - l
  }

  case class CommandLine(
      leftXdf: File = File("left.xdf"),
      rightXdf: File = File("right.xdf"),
      output: File = Path.of("xdf-address-outer-join.csv").toFile
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("XdfAddressCompare"),
      head(
        "Join table/axis addresses from two XDF files and write a CSV for address comparison."
      ),
      help("help").text("Display usage text"),
      opt[File]("left-xdf")
        .required()
        .action((f, c) => c.copy(leftXdf = f))
        .text("Filename of the left XDF"),
      opt[File]("right-xdf")
        .required()
        .action((f, c) => c.copy(rightXdf = f))
        .text("Filename of the right XDF"),
      opt[File]("output")
        .required()
        .action((f, c) => c.copy(output = f))
        .text("Output CSV filename")
    )
  }
}
