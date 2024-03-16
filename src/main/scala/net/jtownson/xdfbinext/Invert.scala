package net.jtownson.xdfbinext

object Invert {

  def invert(axis: Array[BigDecimal], values: Array[BigDecimal]): (Array[BigDecimal], Array[BigDecimal]) = {
    (values, axis)
  }

  def tableInvertX(
      xAxis: Array[BigDecimal],
      yAxis: Array[BigDecimal],
      z: Array[BigDecimal]
  ): (Array[BigDecimal], Array[BigDecimal], Array[BigDecimal]) = {

    // get the range of the data
    val zMin = z.min
    val zMax = z.max
    // divide the range into a new x axis
    val nX    = xAxis.length
    val zAxis = (0 until nX).map(i => zMin + i * (zMax - zMin) / (nX - 1)).toArray

    val x = yAxis.indices.flatMap { iy => // go through each row of the table
      val loadRow   = xAxis
      val torqueRow = z.slice(iy * nX, iy * nX + nX) // extract the row values (torque)
      // here, we have the values of load, but at the wrong torque values
      // so for each torque value we have (zAxis)
      // we need to interpolate
      zAxis.map(tq => LinearInterpolate.linearInterpolate(torqueRow, loadRow, tq))
    }.toArray

    (zAxis, yAxis, x)
  }

}
