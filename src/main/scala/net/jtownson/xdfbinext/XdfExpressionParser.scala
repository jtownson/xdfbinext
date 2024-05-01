package net.jtownson.xdfbinext

class XdfExpressionParser(aliases: Map[String, Interpolated2D]) {

  import fastparse.*
  import NoWhitespace.*

  type Expressable = Interpolated2D | BigDecimal

  private def scalar[$: P]: P[BigDecimal] = P(
    (CharIn("\\-").? ~ CharIn("0-9").rep(1) ~ (CharIn("\\.") ~ CharIn("0-9").rep(1)).?).!.map((s: String) =>
      BigDecimal(s)
    )
  )

  private def table[$: P]: P[Interpolated2D] = (
    CharIn("a-z", "A-Z").!.map(alias => aliases(alias))
  )

  private def parens[$: P]: P[Expressable] = P("(" ~/ addSub ~ ")")

  private def factor[$: P]: P[Expressable] = P(scalar | table | parens)

  private def divMul[$: P]: P[Expressable] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)

  private def addSub[$: P]: P[Expressable] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)

  private def expr[$: P]: P[Expressable] = P(addSub ~ End)

  private def eval(tree: (Expressable, Seq[(String, Expressable)])): Expressable = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) =>
      (left, right) match {
        case (l: BigDecimal, r: BigDecimal) =>
          op match {
            case "+" => l + r
            case "-" => l - r
            case "*" => l * r
            case "/" => l / r
          }

        case (l: Interpolated2D, r: BigDecimal) =>
          op match {
            case "*" => l.multiply(r)
            case "/" => l.divide(r)
          }

        case (l: BigDecimal, r: Interpolated2D) =>
          op match {
            case "*" => r.multiply(l)
            case "/" => r.divide(l)
          }

        case (l: Interpolated2D, r: Interpolated2D) =>
          op match {
            case "+" => l.add(r)
            case "-" => l.subtract(r)
          }
      }
    }
  }

  def parse(expression: String): Expressable = {
    val exprNoSpaces = expression.replace(" ", "")
    fastparse.parse(exprNoSpaces, expr(_)).get.value
  }

  def parseTableExpr(expression: String): Interpolated2D = {
    val exprNoSpaces = expression.replace(" ", "")
    fastparse.parse(exprNoSpaces, expr(_)).get.value match
      case i: Interpolated2D => i
      case x => throw new IllegalArgumentException(s"Expression $expression must reference at least one table.")
  }
}

object XdfExpressionParser {
  val empty: XdfExpressionParser = new XdfExpressionParser(Map.empty)

  def from(aliases: Map[String, Interpolated2D]): XdfExpressionParser = new XdfExpressionParser(aliases)
}
