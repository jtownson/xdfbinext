package net.jtownson.xdfbinext.a2l

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

object RatFun {
  val identity: RatFun = RatFun(0, 1, 0, 0, 0, 1)
}
