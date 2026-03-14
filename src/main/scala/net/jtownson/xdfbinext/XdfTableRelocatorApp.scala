package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.XdfTable
import scopt.{OParser, OParserBuilder}

import java.io.File
import scala.io.Source
import scala.util.Using

object XdfTableRelocatorApp {

  /** CLI for locating relocated table addresses by scanning a target bin with sliding-window SSD matching.
    *
    * Requires:
    *   - an input XDF
    *   - the source bin used with that XDF
    *   - a target bin to search for relocated tables
    */
  def main(args: Array[String]): Unit = {
    val config = OParser.parse(parser, args, CommandLine()).get
    val xdfModel =
      Using.resource(Source.fromFile(config.inputXdf))(xdfResource => XdfParser.parse(xdfResource.mkString))
    val source    = new XDFBinAdapter(config.inputBin, xdfModel)
    val relocator = new XdfTableRelocator(source)

    println("table_name,source_address,best_address,variance,address_diff")
    val tableName = config.tableName
    val table     = xdfModel.tablesByName(tableName)

    relocateTableByName(config, relocator, table)

  }

  private def relocateTableByName(config: CommandLine, relocator: XdfTableRelocator, table: XdfTable): Unit = {
    val sourceAddress = table.axes.z.embeddedData.mmedAddress
    val candidates = relocator
      .relocateAll(
        tableName = table.title,
        targetBin = config.relocateBin,
        topN = config.topN
      )

    candidates.foreach { candidate =>
      val diff             = formatSignedDiff(candidate.address - sourceAddress)
      val sourceAddrHex    = sourceAddress.toHexString.toUpperCase
      val candidateAddrHex = candidate.address.toHexString.toUpperCase
      println(
        s""""${candidate.table}","0x$sourceAddrHex","0x$candidateAddrHex","${candidate.variance}","$diff""""
      )
    }
  }

  private def formatSignedDiff(diff: Long): String = {
    val sign      = if (diff < 0) "-" else ""
    val magnitude = BigInt(diff).abs.toString(16).toUpperCase
    s"${sign}0x${magnitude}"
  }

  case class CommandLine(
      tableName: String = "",
      inputXdf: File = File("model.xdf"),
      inputBin: File = File("source.bin"),
      relocateBin: File = File("relocate.bin"),
      topN: Int = 1
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("XdfTableRelocatorApp"),
      head("Find relocated table addresses in another bin."),
      help("help").text("Display usage text"),
      opt[String]("table-name")
        .required()
        .action((n, c) => c.copy(tableName = n))
        .text("Name of the table to relocate"),
      opt[File]("input-xdf")
        .required()
        .action((f, c) => c.copy(inputXdf = f))
        .text("Input XDF filename"),
      opt[File]("input-bin")
        .required()
        .action((f, c) => c.copy(inputBin = f))
        .text("Bin file corresponding to the input XDF"),
      opt[File]("relocate-bin")
        .required()
        .action((f, c) => c.copy(relocateBin = f))
        .text("Bin file to search for relocated table addresses"),
      opt[Int]("top-n")
        .optional()
        .action((n, c) => c.copy(topN = n))
        .text("Number of candidate addresses to evaluate per table (default: 5)")
    )
  }
}
