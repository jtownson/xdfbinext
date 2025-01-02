package net.jtownson.xdfbinext

import net.alenzen.a2l.Characteristic
import net.jtownson.xdfbinext.A2LBinDiff.Diff
import net.jtownson.xdfbinext.a2l.Reportable.report
import scopt.OParserBuilder

import java.io.{File, PrintStream}
import scala.util.Using

object A2LMapCompare {

  case class CommandLine(
      a2l: File = File("DME861_R1C9J8B3B.a2l"),
      baseBin: File = File("a.bin"),
      modBin: File = File("b.bin"),
      output: Option[File] = None,
      modIgnore: Set[String] = Set.empty
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("A2LMapCompare"),
      head(
        "Compare bin files using an A2L and print out a report with the differences.",
        "Supports iterative tune development by allowing you to see what has and has not been changed when developing a tune.",
        "Also useful for reverse engineering existing bin files."
      ),
      help("help").text("Display usage text"),
      opt[File]("a2l")
        .required()
        .action((x, c) => c.copy(a2l = x))
        .text("A2L model common to both bins"),
      opt[File]("base-bin")
        .required()
        .action((bb, c) => c.copy(baseBin = bb))
        .text("Filename of the starting bin file"),
      opt[File]("mod-bin")
        .required()
        .action((mb, c) => c.copy(modBin = mb))
        .text("Filename of the bin to compare with the base"),
      opt[Set[String]]("mod-ignore")
        .optional()
        .action((l, c) => c.copy(modIgnore = l))
        .text("List of BMW module names to ignore"),
      opt[File]("output")
        .optional()
        .action((of, c) => c.copy(output = Some(of)))
        .text("Output filename for difference report.")
    )
  }

  implicit def csvReader: scopt.Read[Set[String]] = scopt.Read.reads[Set[String]](s => s.split("""\s*,\s*""").toSet)

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val a2l        = config.a2l
        val a2lWrapper = A2LWrapper(a2l.toURI.toURL)

        val baseBin = config.baseBin
        val modBin  = config.modBin

        val diff = A2LBinDiff(baseBin, modBin, a2lWrapper)

        val output: PrintStream = config.output.fold(System.out)(f => new PrintStream(f))

        Using.resource(output) { o =>
          var diffCount = 0

          def isIgnored(c: Characteristic, d: Diff): Boolean = {
            d.summary.referencedBy.forall(mod => config.modIgnore.contains(mod))
          }
          val includedDiffs = diff.diffsByUsage.filterNot(isIgnored)

          o.println(s"Base bin: ${config.baseBin}")
          o.println(s"Modified bin: ${config.modBin}")
          o.println(s"A2L: ${config.a2l}")
          o.println()

          println(s"Reporting ${includedDiffs.size} of ${diff.diffsByUsage.size} differences found.")
          println()

          includedDiffs.foreach { (_, cdiff) =>
            diffCount += 1
            val label         = s"$diffCount."
            val summaryReport = report(label, cdiff.summary)
            val lReport       = report(cdiff.lhs)
            val rReport       = report(cdiff.rhs)
            val diffReport    = report(cdiff.diff)
            o.println(summaryReport)
            o.println(lReport)
            o.println(diffReport)
            o.println(rReport)
          }
        }

      case _ =>
    }
  }
}
