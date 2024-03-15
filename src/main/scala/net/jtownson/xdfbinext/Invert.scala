package net.jtownson.xdfbinext

object Invert {

  def invert(axis: Array[BigDecimal], values: Array[BigDecimal]): (Array[BigDecimal], Array[BigDecimal]) = {
    (values, axis)
  }

  def tableInvertX(
      load: Array[BigDecimal],
      rpm: Array[BigDecimal],
      torque: Array[BigDecimal]
  ): (Array[BigDecimal], Array[BigDecimal], Array[BigDecimal]) = {

    // get the range of the data
    val tqMin = torque.min
    val tqMax = torque.max
    // divide the range into a new x axis
    val nX     = load.length
    val tqAxis = (0 until nX).map(i => tqMin + i * (tqMax - tqMin) / (nX - 1)).toArray

    val ldValues = rpm.indices.flatMap { iy => // go through each row of the table
      val loadRow   = load
      val torqueRow = torque.slice(iy * nX, iy * nX + nX) // extract the row values (torque)
      // here, we have the values of load, but at the wrong torque values
      // so for each torque value we have (tqAxis)
      // we need to interpolate
      tqAxis.map(tq => LinearInterpolate.linearInterpolate(torqueRow, loadRow, tq))
    }.toArray

    (tqAxis, rpm, ldValues)
  }

}
