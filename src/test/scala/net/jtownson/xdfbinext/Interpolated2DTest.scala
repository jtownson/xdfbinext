package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class Interpolated2DTest extends AnyFlatSpec {

  behavior of "Interpolated2D"

  it should "scale a 2D table" in {
    val t = Interpolated2D(
      xAxis = (-2 to 3).map(BigDecimal(_)).toArray,
      yAxis = Array[BigDecimal](1),
      values = (-2 to 3).map(BigDecimal(_) * 2).toArray
    )

    val tt = t.scaleX(1.1)

    val expected = Interpolated2D(
      xAxis = Array("-2.2", "-1.1", "0.0", "1.1", "2.2", "3.3").map(BigDecimal(_)),
      yAxis = Array[BigDecimal](1),
      values = Array("-4", "-2.2", "0.0", "2.2", "4.4", "6").map(BigDecimal(_))
    )

    tt shouldBe expected
  }
}
