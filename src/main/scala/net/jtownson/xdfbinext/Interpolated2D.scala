package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate

import scala.math
import scala.math.BigDecimal.RoundingMode.HALF_UP

case class Interpolated2D(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], values: Array[BigDecimal]) {

  override def equals(obj: Any): Boolean = {
    obj match
      case Interpolated2D(x, y, v) =>
        x.sameElements(xAxis) && y.sameElements(yAxis) && v.sameElements(values)
      case _ =>
        false
  }

  val sizeX: Int = xAxis.length
  val sizeY: Int = yAxis.length

  def map(f: (BigDecimal, BigDecimal, BigDecimal) => (BigDecimal, BigDecimal, BigDecimal)): Interpolated2D = {

    val z0 =
      (new Array[BigDecimal](xAxis.length), new Array[BigDecimal](yAxis.length), new Array[BigDecimal](values.length))

    val t = yAxis.indices.foldLeft(z0) { (acc, iRow) =>
      xAxis.indices.foldLeft(acc) { (accInner, iCol) =>

        val x = xAxis(iCol)
        val y = yAxis(iRow)
        val z = atRowCol(iRow, iCol)

        val (x2, y2, z2) = f(x, y, z)

        val (xAxis2, yAxis2, values2) = accInner

        xAxis2(iCol) = x2
        yAxis2(iRow) = y2
        values2(xAxis.length * iRow + iCol) = z2

        accInner
      }
    }

    Interpolated2D(t._1, t._2, t._3)
  }

  def invertedX: Interpolated2D = {
    val (xp, yp, zp) = Invert.tableInvertX(xAxis, yAxis, values)
    Interpolated2D(xp, yp, zp)
  }

  def atXY(x: BigDecimal, y: BigDecimal): BigDecimal = linearInterpolate(xAxis, yAxis, values, x, y)

  def atRowCol(row: Int, col: Int): BigDecimal = values(xAxis.length * row + col)

  def atRow(row: Int): Array[BigDecimal] = values.slice(xAxis.length * row, xAxis.length * row + xAxis.length)

  def rounded(decimalPlaces: Int): Interpolated2D = {
    Interpolated2D(
      xAxis.map(_.setScale(decimalPlaces, HALF_UP)),
      yAxis.map(_.setScale(decimalPlaces, HALF_UP)),
      values.map(_.setScale(decimalPlaces, HALF_UP))
    )
  }

  def multiply(d: BigDecimal): Interpolated2D = {
    copy(values = values.map(_ * d))
  }

  def divide(d: BigDecimal): Interpolated2D = {
    copy(values = values.map(_ / d))
  }

  def add(r: Interpolated2D): Interpolated2D = {
    require(
      sizeX == r.sizeX && sizeY == r.sizeY,
      s"Addition operations on tables requires matching dimension. Have ($sizeX, $sizeY) vs (${r.sizeX}, ${r.sizeY}."
    )
    copy(values = values.zip(r.values).map((a, b) => a + b))
  }

  def subtract(r: Interpolated2D): Interpolated2D = {
    require(
      sizeX == r.sizeX && sizeY == r.sizeY,
      s"Subtraction operations on tables requires matching dimension. Have ($sizeX, $sizeY) vs (${r.sizeX}, ${r.sizeY}."
    )
    copy(values = values.zip(r.values).map((a, b) => a - b))
  }

  def scaleX(factor: BigDecimal): Interpolated2D = {
    map { (x, y, z) =>
      val x2 = x * factor
      (x2, y, atXY(x2, y))
    }
  }
}

object Interpolated2D {

  def data2Str2D(t: Interpolated2D): String = {
    Data2Str.data2Str2D(t.xAxis.map(_.toString), t.yAxis.map(_.toString), t.values)
  }

  def zero(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal]): Interpolated2D = {
    const(xAxis, yAxis, BigDecimal(0))
  }

  def one(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal]): Interpolated2D = {
    const(xAxis, yAxis, BigDecimal(1))
  }

  def const(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], value: BigDecimal): Interpolated2D = {
    Interpolated2D(xAxis, yAxis, Array.fill(xAxis.length * yAxis.length)(value))
  }
}
