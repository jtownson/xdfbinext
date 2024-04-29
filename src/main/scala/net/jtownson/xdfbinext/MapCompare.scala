package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.{XdfTable1D, XdfTable2D, XdfModel, XdfTable}
import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.io.Source
import scala.util.Using

object MapCompare {
  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val xdf = Using.resource(Source.fromFile(config.xdfModel))(xdfr => XdfParser.parse(xdfr.mkString))

        val notes = config.reportFile.fold(Map.empty[String, String])(notesFile => readNotes(notesFile))

        val allTablesSorted = config.reportFile
          .fold(allTablesByCategory(xdf))(report => allTablesByReport(xdf, report))
          .filterNot(xdf.isBreakpointTable)

        val tablesOrderedAndFiltered =
          TableAndCategoryFilter
            .filterTables(
              tables = allTablesSorted,
              tableExclusions = config.tableExclusions,
              categoryExclusions = config.categoryExclusions
            )

        val comparisons = XDFBinAdapter.compare(xdf, config.baseBin, config.modBin)

        val unmodifiedTables = tablesOrderedAndFiltered.filterNot(table => comparisons.keySet.contains(table.title))
        val excludedTables   = allTablesSorted.filterNot(t => tablesOrderedAndFiltered.contains(t))

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
                  o.print(s"\n=== $tableName ===\n")
                  o.print(s"\n'''Brief description''': ${t.description}\n")
                  o.print(s"\n'''Dimension''': constant\n")
                  o.print(s"\n'''Categories''': ${cats.mkString(", ")}\n")
                  o.print(s"\n'''Units''': ${t.zUnits}\n")
                  o.print('\n')

                case t: XdfTable1D =>
                  o.print(s"\n=== $tableName ===\n")
                  o.print(s"\n'''Brief Description''': ${t.table.description}\n")
                  o.print(s"\n'''Dimension''': 1D, vector\n")
                  o.print(s"\n'''Categories''': ${cats.mkString(", ")}\n")
                  o.print(s"\n'''Unit info''': ${t.table.xUnits} --> ${t.table.zUnits}\n")
                  o.print(s"\n'''Breakpoints''': ${t.xAxisBreakpoints.fold("<labels>")(_.title)}\n")
                  o.print('\n')
                case t: XdfTable2D =>
                  o.print(s"\n=== $tableName ===\n")
                  o.print(s"\n'''Brief description''': ${t.table.description}\n")
                  o.print(s"\n'''Dimension''': 2D, table\n")
                  o.print(s"\n'''Categories''': ${cats.mkString(", ")}\n")
                  o.print(s"\n'''Unit info''': ${t.table.xUnits}, ${t.table.yUnits} --> ${t.table.zUnits}\n")
                  o.print(s"\n'''Breakpoints''': ${t.xAxisBreakpoints.fold("<labels>")(_.title)} vs ${t.yAxisBreakpoints
                      .fold("<labels>")(_.title)}")
                  o.print('\n')
              }

              o.print("\n'''Base''':\n\n")
              o.print(comparison.lhs)
              o.print("\n'''Difference''':\n\n")
              o.print(comparison.diff)
              o.print("\n'''Modified''':\n\n")
              o.print(comparison.rhs)

              notes.get(tableName).foreach { tableNotes =>
                o.print("\n'''Notes''':\n\n")
                o.println(tableNotes)
                o.println()
              }
            }
          }

          o.println("\n== Unmodified tables ==\n")
          unmodifiedTables.foreach { table =>
            o.println(s"\t${table.title} (${table.categoryMems.map(_.category.name).mkString(", ")})\n")
          }

          o.println("\n== Excluded tables ==\n")
          excludedTables.foreach { table =>
            o.println(s"\t${table.title} (${table.categoryMems.map(_.category.name).mkString(", ")})")
          }
        }
      case _ =>
    }
  }

  private def allTablesByCategory(xdf: XdfModel): Seq[XdfTable] = {
    xdf.tables
      .map(table => (table, table.categoryMems.map(_.category).lastOption.map(_.index).getOrElse("0")))
      .sortBy((_, index) => Integer.decode(index))
      .map(_._1)
  }

  private def allTablesByReport(xdf: XdfModel, reportFile: File): Seq[XdfTable] = {
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
