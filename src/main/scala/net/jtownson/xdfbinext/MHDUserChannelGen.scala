package net.jtownson.xdfbinext

import scopt.OParserBuilder

import java.io.{File, PrintStream}
import scala.util.Using

object MHDUserChannelGen {

  case class CommandLine(
      a2l: File = File("DME861_R1C9J8B3B.a2l"),
      output: Option[File] = None,
      measurements: Seq[String] = Seq.empty
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("MHD User channel generator"),
      head(
        "Generates an MHD custom logging channels file from an a2l and a list of measurements."
      ),
      help("help").text("Display usage text"),
      opt[File]("a2l")
        .required()
        .action((x, c) => c.copy(a2l = x))
        .text("An A2L file name"),
      opt[Seq[String]]("measurements")
        .optional()
        .action((l, c) => c.copy(measurements = l))
        .text("List of measurement names from the a2l"),
      opt[File]("output")
        .optional()
        .action((of, c) => c.copy(output = Some(of)))
        .text("Output filename.")
    )
  }

  implicit def csvReader: scopt.Read[Set[String]] = scopt.Read.reads[Set[String]](s => s.split("""\s*,\s*""").toSet)

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val a2l                 = config.a2l
        val userChannelGen      = new Measurement2UserChannel(a2l.toURI.toURL)
        val userChannels        = userChannelGen.measurement2UserChannels(config.measurements.contains)
        val output: PrintStream = config.output.fold(System.out)(f => new PrintStream(f))

        Using.resource(output)(_.print(userChannels))

      case _ =>
    }
  }
}
