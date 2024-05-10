package net.jtownson.xdfbinext

object BACELib {

  //  y(i) = K*(u(i)-y(i-1)) + y(i-1)
  def bace_lowpass_constiv(u: Seq[BigDecimal], k: BigDecimal): Seq[BigDecimal] = {
    def loop(y: Vector[BigDecimal], i: Int): Vector[BigDecimal] = {
      if (i == u.length) {
        y
      } else if (i == 0) {
        loop(y :+ u(i), i + 1)
      } else {
        val yi = k * (u(i) - y(i - 1)) + y(i - 1)
        loop(y :+ yi, i + 1)
      }
    }
    loop(Vector.empty, 0)
  }
}
