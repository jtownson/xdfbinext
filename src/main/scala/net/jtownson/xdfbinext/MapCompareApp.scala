package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.{BinAdapter, XdfParser}
import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.io.Source
import scala.util.Using
import scala.util.matching.{Regex, UnanchoredRegex}

object MapCompareApp {
  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val tableFilters = Set("(Custom)", "(Antilag)", "(Map 2)", "(Map 3)", "(Map 4)", "(FF)", "(FF#2)")

        println(s"xdfModel ${config.xdfModel}, baseBin ${config.baseBin}, modBin ${config.modBin}")
        val xdf         = Using.resource(Source.fromFile(config.xdfModel))(xdfr => XdfParser.parse(xdfr.mkString))
        val comparisons = BinAdapter.compare(xdf, config.baseBin, config.modBin)
        val filteredComparisons = comparisons
          .filter((name, _) => config.tableExpr.matches(name))
          .filterNot((name, _) => tableFilters.exists(filter => name.contains(filter)))

        val output: PrintStream = config.output.fold(System.out)(f => new PrintStream(f))

        Using.resource(output) { o =>
          filteredComparisons.foreach { (table, comparison) =>
            val cats = xdf.tablesByName(table).categoryMems.map(_.category.name)
            o.println(s"TABLE: $table")
            o.println(s"CATEGORIES: ${cats.mkString(", ")}")
            o.println(comparison.lhs)
            o.println(comparison.diff)
            o.println(comparison.rhs)
          }
        }
      case _ =>
    }
  }

  case class CommandLine(
      xdfModel: File = File("model.xdf"),
      baseBin: File = File("a.bin"),
      modBin: File = File("b.bin"),
      tableExpr: Regex = """.+""".r,
      output: Option[File] = None
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("MapCompare"),
      opt[File]("xdf")
        .required()
        .action((x, c) => c.copy(xdfModel = x))
        .text("XDF model common to both bins"),
      opt[File]("base-bin")
        .required()
        .action((bb, c) => c.copy(baseBin = bb))
        .text("filename of the starting bin file"),
      opt[File]("mod-bin")
        .required()
        .action((mb, c) => c.copy(modBin = mb))
        .text("filename of the bin to compare with the base"),
      opt[String]("table-expr")
        .optional()
        .action((e, c) => c.copy(tableExpr = e.r))
        .text("Regular expression matching one or more tables"),
      opt[File]("output")
        .optional()
        .action((of, c) => c.copy(output = Some(of)))
        .text("output filename for difference report")
    )
  }
}
