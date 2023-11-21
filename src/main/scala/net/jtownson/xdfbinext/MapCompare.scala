package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.BinAdapter.BinAdapterCompare
import net.jtownson.xdfbinext.XdfSchema.{Category, Table1DEnriched, Table2DEnriched, XdfModel, XdfTable}
import net.jtownson.xdfbinext.{BinAdapter, XdfParser}
import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.io.Source
import scala.util.Using
import scala.util.matching.{Regex, UnanchoredRegex}

object MapCompare {
  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val xdf = Using.resource(Source.fromFile(config.xdfModel))(xdfr => XdfParser.parse(xdfr.mkString))

        val notes = config.reportFile.fold(Map.empty[String, String])(notesFile => readNotes(notesFile))

        val tablesOrderedAndFiltered =
          TableAndCategoryFilter.filterTables(
            tables = config.reportFile.fold(tablesByCategory(xdf))(report => tablesByReport(xdf, report)),
            tableExclusions = config.tableExclusions,
            categoryExclusions = config.categoryExclusions
          )

        val comparisons = BinAdapter.compare(xdf, config.baseBin, config.modBin)

        val output: PrintStream = config.output.fold(System.out)(f => new PrintStream(f))

        Using.resource(output) { o =>

          o.println(s"Base bin: ${config.baseBin}")
          o.println(s"Modified bin: ${config.modBin}")
          o.println(s"XDF: ${config.xdfModel}")
          o.println()

          tablesOrderedAndFiltered.foreach { table =>
            val tableName       = table.title
            val cats            = table.categoryMems.map(_.category.name)
            val maybeComparison = comparisons.get(tableName)

            maybeComparison.foreach { comparison =>
              xdf.table(tableName) match {
                case t: XdfTable =>
                  o.println(s"Table (scalar): $tableName")
                  o.println(s"Description: ${t.description}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                  o.println(s"Unit info: ${t.zUnits}")
                case t: Table1DEnriched =>
                  o.println(s"Table (vector): $tableName")
                  o.println(s"Description: ${t.table.description}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                  o.println(s"Unit info: ${t.table.xUnits} --> ${t.table.zUnits}")
                  o.println(s"Breakpoints: ${t.xAxisBreakpoints.fold("<labels>")(_.title)}")
                case t: Table2DEnriched =>
                  o.println(s"Table (matrix): $tableName")
                  o.println(s"Description: ${t.table.description}")
                  o.println(s"Categories: ${cats.mkString(", ")}")
                  o.println(s"Unit info: ${t.table.xUnits}, ${t.table.yUnits} --> ${t.table.zUnits}")
                  o.println(s"Breakpoints: ${t.xAxisBreakpoints.fold("<labels>")(_.title)} vs ${t.yAxisBreakpoints
                      .fold("<labels>")(_.title)}")
              }

              o.println("Base:")
              o.println(comparison.lhs)
              o.println("Difference:")
              o.println(comparison.diff)
              o.println("Modified:")
              o.println(comparison.rhs)

              notes.get(tableName).foreach { tableNotes =>
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

  private def tablesByCategory(xdf: XdfModel): Seq[XdfTable] = {
    xdf.tables
      .map(table => (table, table.categoryMems.map(_.category).last))
      .sortBy((_, cat) => Integer.decode(cat.index))
      .map(_._1)
  }

  private def tablesByReport(xdf: XdfModel, reportFile: File): Seq[XdfTable] = {
    Using.resource(Source.fromFile(reportFile)) { reportResource =>
      val reportTables = ReportExtractor.tables(reportResource.getLines().to(Iterable))
      xdf.tables.sortBy { t =>
        val i = reportTables.indexOf(t.title)
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
      tableExclusions: Set[String] = Set.empty,
      categoryExclusions: Set[String] = Set.empty,
      output: Option[File] = None
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  implicit val csvReader: scopt.Read[Set[String]] = scopt.Read.reads[Set[String]](s => s.split("""\s*,\s*""").toSet)

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("MapCompare"),
      head(
        "Compare bin files using an XDF and print out a report with the differences.",
        "This allows you to see what you have and have not done when developing a tune.",
        "It is also useful if reverse engineering a tune."
      ),
      help("help").text("Display usage text"),
      opt[File]("xdf")
        .required()
        .action((x, c) => c.copy(xdfModel = x))
        .text("XDF model common to both bins"),
      opt[File]("base-bin")
        .required()
        .action((bb, c) => c.copy(baseBin = bb))
        .text("Filename of the starting bin file"),
      opt[File]("mod-bin")
        .required()
        .action((mb, c) => c.copy(modBin = mb))
        .text("Filename of the bin to compare with the base"),
      opt[Set[String]]("table-exclusions")
        .optional()
        .action((ss, c) => c.copy(tableExclusions = ss))
        .text(
          "A comma separated list of substrings used to exclude tables. E.g. '(FF), (FF#2)' => exclude flex fuel tables."
        ),
      opt[Set[String]]("category-exclusions")
        .optional()
        .action((ss, c) => c.copy(categoryExclusions = ss))
        .text(
          "A comma separated list of exact names used to exclude categories. E.g. 'MHD+ Suite, MHD+ Config' => exclude these two categories."
        ),
      opt[File]("report")
        .optional()
        .action((r, c) => c.copy(reportFile = Some(r)))
        .text(
          "Filename of an existing report from which to extract notes and table ordering. Enables re-running of a diff without having to rework notes and table re-ordering."
        ),
      opt[File]("output")
        .optional()
        .action((of, c) => c.copy(output = Some(of)))
        .text("Output filename for difference report.")
    )
  }
}
