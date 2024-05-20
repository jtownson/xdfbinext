package net.jtownson.xdfbinext.a2l

import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate

trait CurveType[X, Z] {
  def axis: Array[X]
  def values: Array[Z]
  override def equals(obj: Any): Boolean = {
    obj match
      case mt: CurveType[_, _] =>
        axis.sameElements(mt.axis) && values.sameElements(mt.values)
      case _ =>
        false
  }
}

object CurveType {

  type CurveValueType = NumberStringTable1D | StringNumberTable1D | StringStringTable1D | NumberNumberTable1D

  case class NumberNumberTable1D(axis: Array[BigDecimal], values: Array[BigDecimal])
      extends CurveType[BigDecimal, BigDecimal] {
    def atX(x: BigDecimal): BigDecimal = linearInterpolate(axis, values, x)
  }

  case class StringNumberTable1D(axis: Array[String], values: Array[BigDecimal]) extends CurveType[String, BigDecimal]

  case class StringStringTable1D(axis: Array[String], values: Array[String]) extends CurveType[String, String]

  case class NumberStringTable1D(axis: Array[BigDecimal], values: Array[String]) extends CurveType[BigDecimal, String]

}
