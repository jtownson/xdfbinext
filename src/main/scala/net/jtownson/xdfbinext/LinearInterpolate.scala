package net.jtownson.xdfbinext

object LinearInterpolate {

  def linearInterpolate(axis: Array[BigDecimal], values: Array[BigDecimal], x: BigDecimal): BigDecimal = {
    val i1 = axis.indexWhere(_ >= x)
    if (i1 == 0) values(0)
    else if (i1 == -1) values.last
    else
      val i0       = i1 - 1
      val (x0, x1) = (axis(i0), axis(i1))
      val (y0, y1) = (values(i0), values(i1))

      val m = (y1 - y0) / (x1 - x0)
      val c = y0 - m * x0

      m * x + c
  }

  def linearInterpolate(
      xAxis: Array[BigDecimal],
      yAxis: Array[BigDecimal],
      values: Array[BigDecimal],
      x: BigDecimal,
      y: BigDecimal
  ): BigDecimal = {
    val (i1, j1) = (xAxis.indexWhere(_ >= x), yAxis.indexWhere(_ >= y))
    val (i0, j0) = (i1 - 1, j1 - 1)

    val (x0, x1) = (xAxis(i0), xAxis(i1))
    val (y0, y1) = (yAxis(j0), yAxis(j1))

    val (z00, z10, z01, z11) = (
      values(j0 * xAxis.length + i0),
      values(j0 * xAxis.length + i1),
      values(j1 * xAxis.length + i0),
      values(j1 * xAxis.length + i1)
    )

    val r0 = z00 * (x1 - x) / (x1 - x0) + z10 * (x - x0) / (x1 - x0)

    val r1 = z01 * (x1 - x) / (x1 - x0) + z11 * (x - x0) / (x1 - x0)

    val p = r0 * (y1 - y) / (y1 - y0) + r1 * (y - y0) / (y1 - y0)

    p
  }

}
