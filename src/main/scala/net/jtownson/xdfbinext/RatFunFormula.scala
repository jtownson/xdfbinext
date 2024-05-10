package net.jtownson.xdfbinext

object RatFunFormula {

  case class RatFun(a: BigDecimal, b: BigDecimal, c: BigDecimal, d: BigDecimal, e: BigDecimal, f: BigDecimal) {
    def apply(x: BigDecimal): BigDecimal = {
      if (a == 0 && d == 0)
        applyFormulaInvLinear(x)
      else
        ???
    }

    def applyFormulaInvLinear(x: BigDecimal): BigDecimal = {
      (c - f * x) / (e * x - b)
    }
  }

  def toFormulaInv(
      a: BigDecimal,
      b: BigDecimal,
      c: BigDecimal,
      d: BigDecimal,
      e: BigDecimal,
      f: BigDecimal
  ): String = {

    if (a == 0 && d == 0)
      toFormulaInv(b, c, e, f)
    else
      ???
  }

  def toFormulaInv(
      b: BigDecimal,
      c: BigDecimal,
      e: BigDecimal,
      f: BigDecimal
  ): String = {

    if (e == 0)
      toFormulaInv(b, c, f)
    else
      s"($c-$f*x)/($e*x-$b)"
  }

  def toFormulaInv(
      b: BigDecimal,
      c: BigDecimal,
      f: BigDecimal
  ): String = {
    if (c == 0)
      toFormulaInv(b, f)
    else
      s"($c-$f*x)/-b"
  }

  def toFormulaInv(
      b: BigDecimal,
      f: BigDecimal
  ): String = {
    if (f == 1)
      s"x/$b"
    else
      s"$f*x/$b"
  }
}
