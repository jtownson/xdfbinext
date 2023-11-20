package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.BinAdapter.BinAdapterCompare
import net.jtownson.xdfbinext.XdfSchema.{Category, Table1DEnriched, Table2DEnriched, XdfModel, XdfTable}
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
        val xdf = Using.resource(Source.fromFile(config.xdfModel))(xdfr => XdfParser.parse(xdfr.mkString))

        val notes = config.reportFile.fold(Map.empty[String, String])(notesFile => readNotes(notesFile))

        val tablesOrdered = config.reportFile.fold(tablesByCategory(xdf))(report => tablesByReport(xdf, report))

        val tableFilters = Set("(Custom)", "(Antilag)", "(Map 2)", "(Map 3)", "(Map 4)", "(FF)", "(FF#2)")

        println(s"xdfModel ${config.xdfModel}, baseBin ${config.baseBin}, modBin ${config.modBin}")

        val comparisons = BinAdapter.compare(xdf, config.baseBin, config.modBin)

        val filteredComparisons = comparisons
          .filter((name, _) => config.tableExpr.matches(name))
          .filterNot((name, _) => tableFilters.exists(filter => name.contains(filter)))

        val output: PrintStream = config.output.fold(System.out)(f => new PrintStream(f))

        Using.resource(output) { o =>
          tablesOrdered.foreach { table =>

            val cats            = xdf.tablesByName(table).categoryMems.map(_.category.name)
            val maybeComparison = filteredComparisons.get(table)

            maybeComparison.foreach { comparison =>
              xdf.table(table) match {
                case t: XdfTable =>
                  o.println(s"Table (scalar): $table")
                  o.println(s"Unit info: ${t.zUnits}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                case t: Table1DEnriched =>
                  o.println(s"Table (vector): $table")
                  o.println(s"Unit info: ${t.table.xUnits} --> ${t.table.zUnits}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                  o.println(s"Breakpoints: ${t.xAxisBreakpoints.fold("<labels>")(_.title)}")
                case t: Table2DEnriched =>
                  o.println(s"Table (matrix): $table")
                  o.println(s"Unit info: ${t.table.xUnits}, ${t.table.yUnits} --> ${t.table.zUnits}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                  o.println(s"Breakpoints: ${t.xAxisBreakpoints.fold("<labels>")(_.title)} vs ${t.yAxisBreakpoints
                      .fold("<labels>")(_.title)}")
              }

              o.println("Base:")
              o.println(comparison.lhs)
              o.println("Difference:")
              o.println(comparison.diff)
              o.println("Modified:")
              o.println(comparison.rhs)

              notes.get(table).foreach { tableNotes =>
                o.println("Notes:")
                o.println(tableNotes)
                o.println()
              }
            }
          }
        }
      case _ =>
    }
  }

  private def tablesByCategory(xdf: XdfModel): Seq[String] = {
    xdf.tables
      .map(table => (table.title, table.categoryMems.map(_.category).last))
      .sortBy((tableName, cat) => Integer.decode(cat.index))
      .map(_._1)
  }

  private def tablesByReport(xdf: XdfModel, reportFile: File): Seq[String] = {
    Using.resource(Source.fromFile(reportFile)) { reportResource =>
      val reportTables = ReportExtractor.tables(reportResource.getLines().to(Iterable))
      xdf.tables.map(_.title).sortBy { name =>
        val i = reportTables.indexOf(name)
        if (i == -1) Int.MaxValue else i
      }
    }
  }

  private def readNotes(notesFile: File): Map[String, String] = {
    Using.resource(Source.fromFile(notesFile)) { notesResource =>
      ReportExtractor.notes(notesResource.getLines().to(Iterable))
    }
  }

  case class CommandLine(
      xdfModel: File = File("model.xdf"),
      baseBin: File = File("a.bin"),
      modBin: File = File("b.bin"),
      reportFile: Option[File] = None,
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
      opt[File]("report")
        .optional()
        .action((r, c) => c.copy(reportFile = Some(r))),
      opt[File]("output")
        .optional()
        .action((of, c) => c.copy(output = Some(of)))
        .text("output filename for difference report")
    )
  }
}
