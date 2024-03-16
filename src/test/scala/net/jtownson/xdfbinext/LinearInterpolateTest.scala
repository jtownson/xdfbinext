package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.{Interpolated2D, linearInterpolate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.prop.TableDrivenPropertyChecks.*

class LinearInterpolateTest extends AnyFlatSpec {

  val t1d = Table[String, BigDecimal, BigDecimal](
    ("Description", "x", "expected"),
    ("x falls off to the left", -3.1, 0.0),
    ("x falls within the breakpoints", 0.1, 1.2),
    ("x falls within the breakpoints", 12.0, 68.850)
  )
  val axis: Array[BigDecimal] = Array(-3.0, -2.2, -1.5, 0.0, 2.0, 2.5, 3.0, 3.8, 9.0, 11.0, 13.0, 15.0)
  val fx: Array[BigDecimal]   = Array(0.0, 0.0, 0.0, 1.2, 1.2, 5.1, 20.0, 20.0, 50.2, 60.4, 77.3, 100.0)

  "LinearInterpolate" should "interpolate a 1D fn" in forAll(t1d) { (_, x, expected) =>
    linearInterpolate(axis, fx, x) shouldBe expected
  }

  // format: off
  val t2d = Table[String, BigDecimal, BigDecimal, BigDecimal](
    ("Desc", "x", "y", "expected"),
    ("x and y within the breakpoints", 2.3, 2.4, 26.3),
    ("x falls before", 0.9, 2.4, 25.0),
    ("x falls after", 6, 2.4, 29.0),
    ("y falls before", 1.5, 0.9, 11.5),
    ("y falls after", 1.5, 5.5, 41.5),
    ("x falls before, y falls before", 0.9, 0.9, 11),
    ("x falls after, y falls before", 5.5, 0.9, 15),
    ("x falls before, y falls after", 0.9, 4.5, 41),
    ("x falls after, y falls after", 5.5, 4.5, 45)
  )
  val xAxis: Array[BigDecimal] = Array(1, 2, 3, 4, 5)
  val yAxis: Array[BigDecimal] = Array(1, 2, 3, 4)
  val fxy: Array[BigDecimal] = Array(
    11, 12, 13, 14, 15,
    21, 22, 23, 24, 25,
    31, 32, 33, 34, 35,
    41, 42, 43, 44, 45
  )

  it should "interpolate a 2D fn" in forAll(t2d) { (_, x, y, expected) =>
    linearInterpolate(xAxis = xAxis, yAxis = yAxis, values = fxy, x = x, y = y) shouldBe expected
  }

  it should "generate a zero table" in {
    Interpolated2D.zero(xAxis, yAxis).values.toSeq shouldBe Seq(
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0)
  }

  // format: on

}
