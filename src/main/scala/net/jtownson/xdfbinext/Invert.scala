package net.jtownson.xdfbinext

object Invert {

  def invert(axis: Array[BigDecimal], values: Array[BigDecimal]): (Array[BigDecimal], Array[BigDecimal]) = {
    (values, axis)
  }

  def tableInvertX(
      xAxis: Array[BigDecimal],
      yAxis: Array[BigDecimal],
      values: Array[BigDecimal]
  ): (Array[BigDecimal], Array[BigDecimal], Array[BigDecimal]) = {

    // get the range of the data
    val zMin = values.min
    val zMax = values.max
    // divide the range into a new x axis
    val nX    = xAxis.length
    val zAxis = (0 until nX).map(i => zMin + i * (zMax - zMin)/(nX-1)).toArray

    val zz = yAxis.indices.flatMap { iy =>
      val a = values.slice(iy * xAxis.length, iy * xAxis.length + xAxis.length)
      zAxis.map(z => LinearInterpolate.linearInterpolate(zAxis, xAxis, z))
    }.toArray

    (zAxis, yAxis, zz)
  }

}
