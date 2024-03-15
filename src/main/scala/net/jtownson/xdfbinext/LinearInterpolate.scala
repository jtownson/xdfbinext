package net.jtownson.xdfbinext

object LinearInterpolate {

  case class Interpolated1D(axis: Array[BigDecimal], values: Array[BigDecimal]) {
    def atX(x: BigDecimal): BigDecimal = linearInterpolate(axis, values, x)
  }

  case class Interpolated2D(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], values: Array[BigDecimal]) {
    def atXY(x: BigDecimal, y: BigDecimal): BigDecimal = linearInterpolate(xAxis, yAxis, values, x, y)

    def atRowCol(row: Int, col: Int): BigDecimal = values(xAxis.length * row + col)

    def atRow(row: Int): Array[BigDecimal] = values.slice(xAxis.length * row, xAxis.length * row + xAxis.length)
  }

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
    if (i1 == 0) {   // x is lower than min axis ord
      if (j1 == 0) { // y is lower than min axis ord
        values(0)
      } else if (j1 == -1) { // y is above the max axis ord
        values(xAxis.length * (yAxis.length - 1))
      } else {
        // want to interpolate between the two values on the left
        val (y0, y1) = (yAxis(j0), yAxis(j1))
        val (z0, z1) = (values(xAxis.length * j0), values(xAxis.length * j1))
        val m        = (z1 - z0) / (y1 - y0)
        val c        = z0 - m * y0
        m * y + c
      }
    } else if (i1 == -1) { // x is higher than max axis ord

      if (j1 == 0) { // y is lower than min axis ord
        values(xAxis.length - 1)
      } else if (j1 == -1) { // y is higher than max axis ord
        values.last
      } else {
        val (y0, y1) = (yAxis(j0), yAxis(j1))
        val (z0, z1) = (values(j0 * xAxis.length + xAxis.length - 1), values(j1 * xAxis.length + xAxis.length - 1))
        val m        = (z1 - z0) / (y1 - y0)
        val c        = z0 - m * y0
        m * y + c
      }

    } else {
      if (j1 == 0) {
        val (x0, x1) = (xAxis(i0), xAxis(i1))
        val (z0, z1) = (values(i0), values(i1))
        val m        = (z1 - z0) / (x1 - x0)
        val c        = z0 - m * x0
        m * x + c
      } else if (j1 == -1) {
        val (x0, x1) = (xAxis(i0), xAxis(i1))
        val (z0, z1) =
          (values(xAxis.length * (yAxis.length - 1) + i0), values(xAxis.length * (yAxis.length - 1) + i1))
        val m = (z1 - z0) / (x1 - x0)
        val c = z0 - m * x0
        m * x + c
      } else {
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
  }

}
