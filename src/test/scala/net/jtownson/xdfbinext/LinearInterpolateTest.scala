package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.linearInterpolate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class LinearInterpolateTest extends AnyFlatSpec {

  "LinearInterpolate" should "interpolate a 1D fn" in {
    val axis: Array[BigDecimal] = Array(-3.0, -2.2, -1.5, 0.0, 2.0, 2.5, 3.0, 3.8, 9.0, 11.0, 13.0, 15.0)
    val f: Array[BigDecimal]    = Array(0.0, 0.0, 0.0, 1.2, 1.2, 5.1, 20.0, 20.0, 50.2, 60.4, 77.3, 100.0)

    linearInterpolate(axis, f, -3.1) shouldBe BigDecimal(0.0)
    linearInterpolate(axis, f, 0.1) shouldBe BigDecimal(1.2)
    linearInterpolate(axis, f, 12.0) shouldBe BigDecimal(68.850)
    linearInterpolate(axis, f, 16.0) shouldBe BigDecimal(100.0)
  }

  it should "interpolate a 2D fn" in {

    val x: Array[BigDecimal] = Array(1, 2, 3, 4, 5)
    val y: Array[BigDecimal] = Array(1, 2, 3, 4)

    val f: Array[BigDecimal] = Array(
      11, 12, 13, 14, 15,
      21, 22, 23, 24, 25,
      31, 32, 33, 34, 35,
      41, 42, 43, 44, 45
    )

    linearInterpolate(x, y, f, 2.3, 2.4) shouldBe BigDecimal(26.3)
  }
}
