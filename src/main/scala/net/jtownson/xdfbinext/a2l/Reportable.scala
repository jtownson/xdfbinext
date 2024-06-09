package net.jtownson.xdfbinext.a2l

import net.jtownson.xdfbinext.A2LBinAdapter.CharacteristicValue
import net.jtownson.xdfbinext.Data2Str
import net.jtownson.xdfbinext.a2l.CharacteristicSummary.{CurveSummary, MapSummary, ValBlkSummary, ValueSummary}
import net.jtownson.xdfbinext.a2l.CurveType.{
  NumberNumberTable1D,
  NumberStringTable1D,
  StringNumberTable1D,
  StringStringTable1D
}
import net.jtownson.xdfbinext.a2l.MapType.*

object Reportable {

  def report(label: String, s: CharacteristicSummary): String =
    s match
      case ss: ValueSummary  => report(label, ss)
      case ss: ValBlkSummary => report(label, ss)
      case ss: CurveSummary  => report(label, ss)
      case ss: MapSummary    => report(label, ss)

  def report(cv: CharacteristicValue): String =
    cv match
      case v: BigDecimal =>
        report(v)
      case v: String =>
        report(v)
      case v: NumericArray =>
        report(v)
      case v: StringArray =>
        report(v)
      case v: NumberStringNumberTable2D =>
        report(v)
      case v: StringNumberNumberTable2D =>
        report(v)
      case v: StringNumberTable1D =>
        report(v)
      case v: StringStringTable1D =>
        report(v)
      case v: NumberNumberStringTable2D =>
        report(v)
      case v: StringStringStringTable2D =>
        report(v)
      case v: NumberStringStringTable2D =>
        report(v)
      case v: StringNumberStringTable2D =>
        report(v)
      case v: NumberStringTable1D =>
        report(v)
      case v: NumberNumberTable1D =>
        report(v)
      case v: NumberNumberNumberTable2D =>
        report(v)
      case v: StringStringNumberTable2D =>
        report(v)

  def report(label: String, s: ValueSummary): String = {
    val sb: StringBuilder = new StringBuilder
    sb.append(s"\n=== $label ${s.name} ===\n")
    sb.append(s"\n'''Description''': ${s.description}\n")
    sb.append(s"\n'''Type''': value\n")
    sb.append(s"\n'''Usage''': ${s.referencedBy.mkString(", ")}\n")
    sb.append(s"\n'''Units''': ${s.fnUnits}\n")
    sb.toString
  }

  def report(label: String, s: ValBlkSummary): String = {
    val sb: StringBuilder = new StringBuilder
    sb.append(s"\n=== $label ${s.name} ===\n")
    sb.append(s"\n'''Description''': ${s.description}\n")
    sb.append(s"\n'''Type''': value\n")
    sb.append(s"\n'''Usage''': ${s.referencedBy.mkString(", ")}\n")
    sb.append(s"\n'''Units''': ${s.fnUnits}\n")
    sb.toString
  }

  def report(label: String, s: CurveSummary): String = {
    val sb: StringBuilder = new StringBuilder
    sb.append(s"\n=== $label ${s.name} ===\n")
    sb.append(s"\n'''Description''': ${s.description}\n")
    sb.append(s"\n'''Type''': curve\n")
    sb.append(s"\n'''Usage''': ${s.referencedBy.mkString(", ")}\n")
    sb.append(s"\n'''Axis Units''': ${s.axisUnits}\n")
    sb.append(s"\n'''Units''': ${s.fnUnits}\n")
    sb.toString
  }

  def report(label: String, s: MapSummary): String = {
    val sb: StringBuilder = new StringBuilder
    sb.append(s"\n=== $label ${s.name} ===\n")
    sb.append(s"\n'''Description''': ${s.description}\n")
    sb.append(s"\n'''Type''': curve\n")
    sb.append(s"\n'''Usage''': ${s.referencedBy.mkString(", ")}\n")
    sb.append(s"\n'''Axis Units (X)''': ${s.xAxisUnits}\n")
    sb.append(s"\n'''Axis Units (Y)''': ${s.yAxisUnits}\n")
    sb.append(s"\n'''Units''': ${s.fnUnits}\n")
    sb.toString
  }

  def report(v: NumberNumberNumberTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: NumberNumberStringTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: NumberStringNumberTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: NumberStringStringTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: StringNumberStringTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: StringNumberNumberTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: StringStringStringTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: StringStringNumberTable2D): String =
    Data2Str.data2Str2D(v.xAxis.map(_.toString), v.yAxis.map(_.toString), v.values.map(_.toString))

  def report(v: NumberNumberTable1D): String =
    Data2Str.data2Str1D(v.axis.map(_.toString), v.values.map(_.toString))

  def report(v: StringNumberTable1D): String =
    Data2Str.data2Str1D(v.axis.map(_.toString), v.values.map(_.toString))

  def report(v: StringStringTable1D): String =
    Data2Str.data2Str1D(v.axis.map(_.toString), v.values.map(_.toString))

  def report(v: NumberStringTable1D): String =
    Data2Str.data2Str1D(v.axis.map(_.toString), v.values.map(_.toString))

  def report(v: NumericArray): String =
    v.values.mkString("   ")

  def report(v: StringArray): String =
    v.values.mkString("   ")

  def report(v: BigDecimal): String =
    v.toString

  def report(v: String): String = v
}
