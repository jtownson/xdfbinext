package net.jtownson.xdfbinext

import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.io.Source
import scala.util.Using

object XDFMapApply {

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val xdfFrom = Using.resource(Source.fromFile(config.xdfModelFrom))(xdfr => XdfParser.parse(xdfr.mkString))

        val xdfTo = Using.resource(Source.fromFile(config.xdfModelTo))(xdfr => XdfParser.parse(xdfr.mkString))

        val plan = readPlan(config.planFile)

        val binFrom = new XDFBinAdapter(config.binFrom, xdfFrom, "r")

        val binTo = new XDFBinAdapter(config.binTo, xdfTo, "rwd")

        plan.foreach { table =>
          val xdfTableFrom = xdfFrom.tablesByName(table)
          val data         = binFrom.readRaw(xdfTableFrom)

          val xdfTableTo = xdfTo.tablesByName(table)

          binTo.writeRaw(xdfTableTo.axes.z.embeddedData.mmedAddress, data)
        }
    }
  }

  private def readPlan(planFile: File): List[String] = {
    Using.resource(Source.fromFile(planFile))(_.getLines().toList)
  }

  case class CommandLine(
      xdfModelFrom: File = File("model-from.xdf"),
      xdfModelTo: File = File("model-to.xdf"),
      binFrom: File = File("a.bin"),
      binTo: File = File("b.bin"),
      planFile: File = File("plan.txt")
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  implicit val csvReader: scopt.Read[Set[String]] = scopt.Read.reads[Set[String]](s => s.split("""\s*,\s*""").toSet)

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("XDFMapApply"),
      head(
        "Write table data from one bin to another using a list of table names in a plan file."
      ),
      help("help").text("Display usage text"),
      opt[File]("xdf-from")
        .required()
        .action((x, c) => c.copy(xdfModelFrom = x))
        .text("XDF model for the from bin"),
      opt[File]("xdf-to")
        .required()
        .action((x, c) => c.copy(xdfModelTo = x))
        .text("XDF model for the to bin"),
      opt[File]("bin-from")
        .required()
        .action((binFrom, c) => c.copy(binFrom = binFrom))
        .text("Filename of the source bin file"),
      opt[File]("bin-to")
        .required()
        .action((binTo, c) => c.copy(binTo = binTo))
        .text("Filename of the bin to modify"),
      opt[File]("plan-file")
        .action((planFile, c) => c.copy(planFile = planFile))
        .text(
          "Filename containing a list of table names separated by newlines."
        )
    )
  }
}
