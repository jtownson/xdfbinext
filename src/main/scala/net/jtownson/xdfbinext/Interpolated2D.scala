package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate

import scala.math.BigDecimal.RoundingMode.HALF_UP

case class Interpolated2D(xAxis: Array[BigDecimal], yAxis: Array[BigDecimal], values: Array[BigDecimal]) {

  val sizeX: Int = xAxis.length
  val sizeY: Int = yAxis.length

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
