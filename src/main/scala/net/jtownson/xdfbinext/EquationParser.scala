package net.jtownson.xdfbinext

object EquationParser:

  import fastparse._, NoWhitespace._

  class EquationParser[T](lift: T => BigDecimal):

    private def plusOrMinus[$: P]               = P(CharIn("+\\-"))
    private def divideOrMultiply[$: P]          = P(CharIn("+\\-"))
    private def sign[$: P]: P[Int]              = P(plusOrMinus.?.!).map(toSign)
    private def digits[$: P]: P[BigDecimal]     = CharIn("0-9").rep(1).!.map(BigDecimal(_))
    private def fractional[$: P]: P[BigDecimal] = P("." ~ CharIn("0-9").rep(1).!).map { s => BigDecimal(s"0.$s") }
    private def integral[$: P]: P[BigDecimal]   = P(sign ~ digits).map((sign, bd) => bd * sign)
    private def exponent[$: P]: P[BigDecimal]   = P(CharIn("eE") ~ integral).map(toExponent)

    private def toSign(s: String): Int =
      if (s.isBlank || s == "+") 1 else if (s == "-") -1 else throw new IllegalStateException()

    private def toIntegral(s: String): BigDecimal = BigDecimal(s)

    private def toFractional(s: String): BigDecimal = BigDecimal(s"0.$s")

    private def toExponent(bd: BigDecimal): BigDecimal = BigDecimal(10).pow(bd.intValue)

    def numberS2[$: P]: P[BigDecimal] = P(integral.? ~ fractional ~ exponent.?).map {
      (maybeIntegral, fractional, maybeExponent) =>
        val integral = maybeIntegral.getOrElse(BigDecimal(0))
        val exponent = maybeExponent.getOrElse(BigDecimal(1))
        (integral + fractional) * exponent
    }

    def numberS1[$: P]: P[BigDecimal] = P(integral ~ fractional.? ~ exponent.?).map {
      (integral, maybeFractional, maybeExponent) =>
        val i: BigDecimal = integral
        val fractional    = maybeFractional.getOrElse(BigDecimal(0))
        val exponent      = maybeExponent.getOrElse(BigDecimal(1))
        (integral + fractional) * exponent
    }

    def number[$: P]: P[BigDecimal] = P(numberS1 | numberS2)

    def parens[$: P]: P[BigDecimal] = P("(" ~/ addSub ~ ")")

    def factor[$: P]: P[BigDecimal] = P(number | parens)

    def divMulRedundantParens[$: P]: P[BigDecimal] = P("(" ~/ divMulBare ~ ")")

    def divMulBare[$: P]: P[BigDecimal] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

    def divMul[$: P]: P[BigDecimal] = P(divMulRedundantParens | divMulBare)

    def addSub[$: P]: P[BigDecimal] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)

    def expr[$: P]: P[BigDecimal] = P((parens | addSub) ~ End)

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

    private def eval(v: (BigDecimal, String, BigDecimal)): BigDecimal = {
      val (left, op, right) = v
      op match {
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case "/" => left / right
      }
    }

    def doParse(e: String)(parser: P[Any] => P[BigDecimal]): BigDecimal =
      parse(e.replaceAll("\\s*", ""), parser(_)).get.value

    def parseF0(e: String): BigDecimal = {
      parse(e.replaceAll("\\s*", ""), expr(_)).get.value
    }

    def parseF1(e: String): T => BigDecimal = { (x: T) =>
      val ee =
        e.replace("x", x.toString).replace("X", x.toString).replace("(", "").replace(")", "").replaceAll("\\s*", "")
      parseF0(ee)
    }

  private val equationParserByte   = new EquationParser[Byte](BigDecimal(_))
  private val equationParserShort  = new EquationParser[Short](BigDecimal(_))
  private val equationParserInt    = new EquationParser[Int](BigDecimal(_))
  private val equationParserLong   = new EquationParser[Long](BigDecimal(_))
  private val equationParserBigInt = new EquationParser[BigInt](BigDecimal(_))
  val equationParserBigDecimal     = new EquationParser[BigDecimal](bd => bd)

  def parseConst(e: String): BigDecimal = equationParserBigDecimal.parseF0(e)

  def parseByteF1(e: String): Byte => BigDecimal = equationParserByte.parseF1(e)

  def parseShortF1(e: String): Short => BigDecimal = equationParserShort.parseF1(e)

  def parseIntF1(e: String): Int => BigDecimal = equationParserInt.parseF1(e)

  def parseLongF1(e: String): Long => BigDecimal = equationParserLong.parseF1(e)

  def parseBigIntF1(e: String): BigInt => BigDecimal = equationParserBigInt.parseF1(e)

  def parseBigDecimalF1(e: String): BigDecimal => BigDecimal = equationParserBigDecimal.parseF1(e)
