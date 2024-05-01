package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate


case class Interpolated1D(axis: Array[BigDecimal], values: Array[BigDecimal]) {
  def atX(x: BigDecimal): BigDecimal = linearInterpolate(axis, values, x)
}
