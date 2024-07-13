package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2L2Dot.GraphOptions
import scopt.{OParser, OParserBuilder}

import java.io.*
import scala.util.matching.Regex

object A2L2SVG {

  private def fnPred(name: String)(s: String): Boolean = {
    val b: Boolean = s == name
    b
  }

  def main(args: Array[String]): Unit = {
    OParser.parse(parser, args, CommandLine()) match {
      case Some(config) =>
        val a2l2Dot    = new A2L2Dot(config.a2lFile.toURI.toURL)
        val objectName = config.objectName
        if (config.isFnCentred)
          A2L2Dot.functionCentredGraphWith(
            config.objectName,
            a2l2Dot,
            _ == objectName,
            _ => true,
            (_, _) => true,
            GraphOptions(showAxes = true, showLocals = true),
            config.outFile.toString
          )
        else
          A2L2Dot.valueCentredGraphWith(config.objectName, a2l2Dot, _ == objectName, config.outFile.toString)

      case _ =>

    }
  }

  case class CommandLine(
      isFnCentred: Boolean = false,
      a2lFile: File = File("model.a2l"),
      objectName: String = "",
      outFile: File = File("model.svg")
  )

  import scopt.OParser

  private val builder: OParserBuilder[CommandLine] = OParser.builder[CommandLine]

  implicit val regexReader: scopt.Read[Regex] = scopt.Read.reads[Regex](s => Regex(s))

  private val parser: OParser[Unit, CommandLine] = {
    import builder.*
    OParser.sequence(
      programName("a2l2svg"),
      head(
        "Creates a graph based around an a2l function, characteristic or measurement."
      ),
      help("help").text("Display usage text"),
      opt[Unit]('f', "function")
        .optional()
        .text(
          "Use this option to graph a function with its inputs and outputs. Otherwise graphs the named characteristic/measurement"
        )
        .action((_, c) => c.copy(isFnCentred = true)),
      opt[File]("a2l")
        .required()
        .action((a, c) => c.copy(a2lFile = a))
        .text("Filename for a2l to parse"),
      opt[String]("object-name")
        .required()
        .action((name, c) => c.copy(objectName = name))
        .text(
          "name of function if -f is set or other object otherwise."
        ),
      opt[File]("out")
        .required()
        .action((o, c) => c.copy(outFile = o))
        .text("output file")
    )
  }
}
