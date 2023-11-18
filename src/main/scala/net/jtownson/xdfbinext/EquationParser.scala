package net.jtownson.xdfbinext

object EquationParser:

  import fastparse._, NoWhitespace._

  class EquationParser[T](lift: T => BigDecimal):

    private def numberShortForm[$: P]: P[BigDecimal] = P(
      (CharIn("\\-").? ~ (CharIn("\\.") ~ CharIn("0-9").rep(1))).!.map((s: String) => BigDecimal(s))
    )

    private def numberLongForm[$: P]: P[BigDecimal] = P(
      (CharIn("\\-").? ~ CharIn("0-9").rep(1) ~ (CharIn("\\.") ~ CharIn("0-9").rep(1)).?).!.map((s: String) =>
        BigDecimal(s)
      )
    )

    private def number[$: P]: P[BigDecimal] = P(numberShortForm | numberLongForm)

    private def parens[$: P]: P[BigDecimal] = P("(" ~/ addSub ~ ")")

    private def factor[$: P]: P[BigDecimal] = P(number | parens)

    private def divMul[$: P]: P[BigDecimal] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

    private def addSub[$: P]: P[BigDecimal] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)

    private def expr[$: P]: P[BigDecimal] = P(addSub ~ End)

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

    def parseF0(e: String): BigDecimal = parse(e, expr(_)).get.value

    def parseF1(e: String): T => BigDecimal = { (x: T) =>
      val ee = e.replace("x", x.toString).replace("X", x.toString)
      parseF0(ee)
    }

  private val equationParserByte       = new EquationParser[Byte](BigDecimal(_))
  private val equationParserShort      = new EquationParser[Short](BigDecimal(_))
  private val equationParserInt        = new EquationParser[Int](BigDecimal(_))
  private val equationParserLong       = new EquationParser[Long](BigDecimal(_))
  private val equationParserBigInt     = new EquationParser[BigInt](BigDecimal(_))
  private val equationParserBigDecimal = new EquationParser[BigDecimal](bd => bd)

  def parseConst(e: String): BigDecimal = equationParserBigDecimal.parseF0(e)

  def parseByteF1(e: String): Byte => BigDecimal = equationParserByte.parseF1(e)

  def parseShortF1(e: String): Short => BigDecimal = equationParserShort.parseF1(e)

  def parseIntF1(e: String): Int => BigDecimal = equationParserInt.parseF1(e)

  def parseLongF1(e: String): Long => BigDecimal = equationParserLong.parseF1(e)

  def parseBigIntF1(e: String): BigInt => BigDecimal = equationParserBigInt.parseF1(e)

  def parseBigDecimalF1(e: String): BigDecimal => BigDecimal = equationParserBigDecimal.parseF1(e)
