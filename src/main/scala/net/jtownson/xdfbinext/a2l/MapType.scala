package net.jtownson.xdfbinext.a2l

import net.jtownson.xdfbinext.{Data2Str, Interpolated2D}
import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate

trait MapType[X, Y, Z] {
  def xAxis: Array[X]
  def yAxis: Array[Y]
  def values: Array[Z]
  override def equals(obj: Any): Boolean = {
    obj match
      case mt: MapType[_, _, _] =>
        xAxis.sameElements(mt.xAxis) && yAxis.sameElements(mt.yAxis) && values.sameElements(mt.values)
      case _ =>
        false
  }
}

object MapType {

  type MapValueType = NumberNumberNumberTable2D | NumberNumberStringTable2D | NumberStringNumberTable2D |
    NumberStringStringTable2D | StringNumberStringTable2D | StringNumberNumberTable2D | StringStringStringTable2D |
    StringStringNumberTable2D

  case class NumberNumberNumberTable2D(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], values: Array[BigDecimal])
      extends MapType[BigDecimal, BigDecimal, BigDecimal] {

    val sizeX: Int = xAxis.length
    val sizeY: Int = yAxis.length

    def atXY(x: BigDecimal, y: BigDecimal): BigDecimal = linearInterpolate(xAxis, yAxis, values, x, y)

    def atRowCol(row: Int, col: Int): BigDecimal = values(xAxis.length * row + col)

    def atRow(row: Int): Array[BigDecimal] = values.slice(xAxis.length * row, xAxis.length * row + xAxis.length)

    def multiply(d: BigDecimal): NumberNumberNumberTable2D = {
      copy(values = values.map(_ * d))
    }

    def divide(d: BigDecimal): NumberNumberNumberTable2D = {
      copy(values = values.map(_ / d))
    }

    def add(r: NumberNumberNumberTable2D): NumberNumberNumberTable2D = {
      require(
        sizeX == r.sizeX && sizeY == r.sizeY,
        s"Addition operations on tables requires matching dimension. Have ($sizeX, $sizeY) vs (${r.sizeX}, ${r.sizeY}."
      )
      copy(values = values.zip(r.values).map((a, b) => a + b))
    }

    def subtract(r: NumberNumberNumberTable2D): NumberNumberNumberTable2D = {
      require(
        sizeX == r.sizeX && sizeY == r.sizeY,
        s"Subtraction operations on tables requires matching dimension. Have ($sizeX, $sizeY) vs (${r.sizeX}, ${r.sizeY}."
      )
      copy(values = values.zip(r.values).map((a, b) => a - b))
    }
  }

  object NumberNumberNumberTable2D {

    def data2Str2D(t: NumberNumberNumberTable2D): String = {
      Data2Str.data2Str2D(t.xAxis.map(_.toString), t.yAxis.map(_.toString), t.values)
    }

    def zero(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal]): NumberNumberNumberTable2D = {
      const(xAxis, yAxis, BigDecimal(0))
    }

    def one(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal]): NumberNumberNumberTable2D = {
      const(xAxis, yAxis, BigDecimal(1))
    }

    def const(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], value: BigDecimal): NumberNumberNumberTable2D = {
      NumberNumberNumberTable2D(xAxis, yAxis, Array.fill(xAxis.length * yAxis.length)(value))
    }
  }

  case class NumberNumberStringTable2D(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], values: Array[String])
      extends MapType[BigDecimal, BigDecimal, String]

  case class NumberStringNumberTable2D(xAxis: Array[BigDecimal], yAxis: Array[String], values: Array[BigDecimal])
      extends MapType[BigDecimal, String, BigDecimal]

  case class NumberStringStringTable2D(xAxis: Array[BigDecimal], yAxis: Array[String], values: Array[String])
      extends MapType[BigDecimal, String, String]

  case class StringNumberStringTable2D(xAxis: Array[String], yAxis: Array[BigDecimal], values: Array[String])
      extends MapType[String, BigDecimal, String]

  case class StringNumberNumberTable2D(xAxis: Array[String], yAxis: Array[BigDecimal], values: Array[BigDecimal])
      extends MapType[String, BigDecimal, BigDecimal]

  case class StringStringStringTable2D(xAxis: Array[String], yAxis: Array[String], values: Array[String])
      extends MapType[String, String, String]

  case class StringStringNumberTable2D(xAxis: Array[String], yAxis: Array[String], values: Array[BigDecimal])
      extends MapType[String, String, BigDecimal]

}
