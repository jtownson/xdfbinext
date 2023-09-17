package net.jtownson.xdfbinext

object EquationParser:

  import fastparse._, NoWhitespace._

  class EquationParser[T](downCast: BigDecimal => T):
    private def number[$: P]: P[BigDecimal] = P(
      (CharIn("\\-").? ~ CharIn("0-9").rep(1) ~ (CharIn("\\.") ~ CharIn("0-9").rep(1)).?).!.map(
        (s: String) => BigDecimal(s)
      )
    )

    private def parens[$: P]: P[BigDecimal] = P("(" ~/ addSub ~ ")")

    private def factor[$: P]: P[BigDecimal] = P(number | parens)

    private def divMul[$: P]: P[BigDecimal] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

    private def addSub[$: P]: P[BigDecimal] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)

    private def expr[$: P]: P[BigDecimal] = P(addSub ~ End)

    private def castExpr[$: P]: P[T] = expr.map(bd => downCast(bd))

    private def eval(tree: (BigDecimal, Seq[(String, BigDecimal)])): BigDecimal = {
      val (base, ops) = tree
      ops.foldLeft(base) { case (left, (op, right)) =>
        op match {
          case "+" => left + right
          case "-" => left - right
          case "*" => left * right
          case "/" => left / right
        }
      }
    }

    def parseF0(e: String): T = parse(e, castExpr(_)).get.value

    def parseF1(e: String): T => T = { (x: T) =>
      val ee = e.replace("x", x.toString).replace("X", x.toString)
      println(s"ee is $ee")
      parseF0(ee)
    }

  private val equationParserShort      = new EquationParser[Short](_.toShort)
  private val equationParserInt        = new EquationParser[Int](_.toInt)
  private val equationParserLong       = new EquationParser[Long](_.toLong)
  private val equationParserBigInt     = new EquationParser[BigInt](_.toBigInt)
  private val equationParserBigDecimal = new EquationParser[BigDecimal](bd => bd)

  def parseShort(e: String): Short            = equationParserShort.parseF0(e)
  def parseShortF1(e: String): Short => Short = equationParserShort.parseF1(e)

  def parseInt(e: String): Int          = equationParserInt.parseF0(e)
  def parseIntF1(e: String): Int => Int = equationParserInt.parseF1(e)

  def parseLong(e: String): Long           = equationParserLong.parseF0(e)
  def parseLongF1(e: String): Long => Long = equationParserLong.parseF1(e)

  def parseBigInt(e: String): BigInt             = equationParserBigInt.parseF0(e)
  def parseBigIntF1(e: String): BigInt => BigInt = equationParserBigInt.parseF1(e)

  def parseBigDecimal(e: String): BigDecimal                 = equationParserBigDecimal.parseF0(e)
  def parseBigDecimalF1(e: String): BigDecimal => BigDecimal = equationParserBigDecimal.parseF1(e)
