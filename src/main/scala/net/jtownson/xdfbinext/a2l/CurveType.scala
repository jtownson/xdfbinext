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

  def foldCurveValueType[T](
      fnst: NumberStringTable1D => T,
      fsnt: StringNumberTable1D => T,
      fsst: StringStringTable1D => T,
      fnnt: NumberNumberTable1D => T
  )(
      value: CurveValueType
  ): T = value match {
    case nst: NumberStringTable1D =>
      fnst(nst)
    case snt: StringNumberTable1D =>
      fsnt(snt)
    case sst: StringStringTable1D =>
      fsst(sst)
    case nnt: NumberNumberTable1D =>
      fnnt(nnt)
  }

  case class NumberNumberTable1D(axis: Array[BigDecimal], values: Array[BigDecimal])
      extends CurveType[BigDecimal, BigDecimal] {
    def atX(x: BigDecimal): BigDecimal = linearInterpolate(axis, values, x)
  }

  case class StringNumberTable1D(axis: Array[String], values: Array[BigDecimal]) extends CurveType[String, BigDecimal]

  case class StringStringTable1D(axis: Array[String], values: Array[String]) extends CurveType[String, String]

  case class NumberStringTable1D(axis: Array[BigDecimal], values: Array[String]) extends CurveType[BigDecimal, String]

}
