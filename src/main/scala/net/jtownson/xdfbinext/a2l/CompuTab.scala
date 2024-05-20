package net.jtownson.xdfbinext.a2l

import net.jtownson.xdfbinext.a2l.CurveType.NumberNumberTable1D

case class CompuTab(x: Array[BigDecimal], fx: Array[BigDecimal]) {
  val interpolated: NumberNumberTable1D = NumberNumberTable1D(x, fx)
}
