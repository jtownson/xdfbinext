package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.Invert.tableInvertX
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class InvertTest extends AnyFlatSpec {

  behavior of "Invert"

  it should "invert a vector where f(x) = x^2" in {
    val axis   = (0 to 10).map(BigDecimal(_)).toArray
    val values = axis.map(i => i * i)

    val (newAxis, newValues) = Invert.invert(axis, values)

    newAxis.toSeq shouldBe Seq(0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100)
    newValues.toSeq shouldBe Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }

  it should "invert a table where f(x,y) = x^2" in {
    val xaxis = (0 to 10).map(BigDecimal(_)).toArray
    val yaxis = (0 to 2).map(BigDecimal(_)).toArray
    val values = for {
      y <- yaxis
      x <- xaxis
    } yield x * x

    val (x2, y2, z2) = tableInvertX(xaxis, yaxis, values)

    x2.toSeq shouldBe Seq(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    y2.toSeq shouldBe Seq(0, 1, 2)
    z2.toSeq shouldBe Seq(
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  }
}
