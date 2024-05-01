package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.*
import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

/** Prints table notes suitable for adding to b58.wiki
  */
object TableNotes {
  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val xdf = Using.resource(Source.fromFile(config.xdfModel))(xdfr => XdfParser.parse(xdfr.mkString))

        val notes = config.reportFile.fold(Map.empty[String, String])(notesFile => readNotes(notesFile))

        val allTablesSorted = config.reportFile
          .fold(allTablesByCategory(xdf))(report => allTablesByCategory(xdf))
          .filterNot(xdf.isBreakpointTable)

        val tablesOrderedAndFiltered =
          TableAndCategoryFilter
            .filterTables(
              tables = allTablesSorted,
              tableExclusions = config.tableExclusions,
              categoryExclusions = config.categoryExclusions
            )

        val comparisons = BinAdapterCompare.compare(xdf, config.baseBin, config.modBin)

        val unmodifiedTables = tablesOrderedAndFiltered.filterNot(table => comparisons.keySet.contains(table.title))
        val excludedTables   = allTablesSorted.filterNot(t => tablesOrderedAndFiltered.contains(t))

        val catMap: mutable.Map[String, StringBuilder] = mutable.Map.empty[String, StringBuilder]

        tablesOrderedAndFiltered.foreach { table =>
          val tableName = table.title
          val cats      = table.categoryMems.map(_.category.name)
          val mainCat   = cats.head
          val sb        = catMap.getOrElse(mainCat, new StringBuilder())

          val maybeComparison = comparisons.get(tableName)
          sb.append('\n')

          doTable(sb, xdf, table)

          maybeComparison.foreach { comparison =>
            sb.append(s"'''Example''':\n\n")
            sb.append(s"${comparison.lhs}\n")
            sb.append("\n\n")
          }

          notes.get(tableName).foreach { tableNotes =>
            sb.append(s"'''Notes''':\n\n")
            sb.append(s"$tableNotes\n")
            sb.append('\n')
          }

          catMap.put(mainCat, sb)
        }

        List(
          "Load",
          "Limits",
          "Throttle",
          "MAF",
          "Boost",
          "Fuel",
          "Ignition",
          "Cooling",
          "Vanos",
          "MHD+ Suite",
          "Exhaust",
          "Dev",
          "Toggles"
        ).foreach { cat =>
          println(s"== $cat ==")
          println(catMap(cat).toString())
        }

//          unmodifiedTables.foreach { table =>
//            doTable(o, xdf, table)
//          }
//
//          excludedTables.foreach { table =>
//            doTable(o, xdf, table)
//          }
      case _ =>
    }
  }

  private def doTable(sb: StringBuilder, xdfModel: XdfModel, table: XdfTable): Unit = {
    val tableName = table.title
    val cats      = table.categoryMems.map(_.category.name)

    xdfModel.table(tableName) match {
      case t: XdfTable =>
        sb.append(s"=== $tableName ===\n")
        sb.append(s"'''Brief description''': ${t.description}\n")
        sb.append('\n')
        sb.append(s"'''Dimension''': constant\n")
        sb.append('\n')
        sb.append(s"'''Categories''': ${cats.mkString(", ")}\n")
        sb.append('\n')
        sb.append(s"'''Units''': ${t.zUnits}\n")
        sb.append('\n')
      case t: XdfTable1D =>
        sb.append(s"=== $tableName ===\n")
        sb.append(s"'''Brief Description''': ${t.table.description}\n")
        sb.append('\n')
        sb.append(s"'''Dimension''': 1D, vector\n")
        sb.append('\n')
        sb.append(s"'''Categories''': ${cats.mkString(", ")}\n")
        sb.append('\n')
        sb.append(s"'''Unit info''': ${t.table.xUnits} --> ${t.table.zUnits}\n")
        sb.append('\n')
        sb.append(s"'''Breakpoints''': ${t.xAxisBreakpoints.fold("<labels>")(_.title)}\n")
        sb.append('\n')
      case t: XdfTable2D =>
        sb.append(s"=== $tableName ===\n")
        sb.append(s"'''Brief description''': ${t.table.description}\n")
        sb.append('\n')
        sb.append(s"'''Dimension''': 2D, table\n")
        sb.append('\n')
        sb.append(s"'''Categories''': ${cats.mkString(", ")}\n")
        sb.append('\n')
        sb.append(s"'''Unit info''': ${t.table.xUnits}, ${t.table.yUnits} --> ${t.table.zUnits}\n")
        sb.append('\n')
        sb.append(s"'''Breakpoints''': ${t.xAxisBreakpoints.fold("<labels>")(_.title)} vs ${t.yAxisBreakpoints
            .fold("<labels>")(_.title)}")
        sb.append('\n')
    }
    sb.append('\n')
  }

  private def allTablesByCategory(xdf: XdfModel): Seq[XdfTable] = {
    xdf.tables
      .map(table => (table, table.categoryMems.map(_.category).last))
      .sortBy((_, cat) => Integer.decode(cat.index))
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
