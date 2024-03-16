package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.LinearInterpolate.Interpolated2D
import org.scalatest.Inside.inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class XdfExpressionParserTest extends AnyFlatSpec {

  behavior of "XdfExpressionParser"

  it should "evaluate a single scalar" in {
    XdfExpressionParser.empty.parse("5") shouldBe BigDecimal(5)
  }

  it should "evaluate a scalar expression" in {
    XdfExpressionParser.empty.parse("10 + 1.2") shouldBe BigDecimal("11.2")
    XdfExpressionParser.empty.parse("10 / 2.5") shouldBe BigDecimal("4")
    XdfExpressionParser.empty.parse("10 * 1.1") shouldBe BigDecimal("11")
    XdfExpressionParser.empty.parse("10 - 1.2") shouldBe BigDecimal("8.8")
  }

  // format: off
  val xAxis: Array[BigDecimal] = Array(1, 2, 3, 4, 5)
  val yAxis: Array[BigDecimal] = Array(1, 2, 3, 4)
  val ft: Array[BigDecimal] = Array(
    11, 12, 13, 14, 15,
    21, 22, 23, 24, 25,
    31, 32, 33, 34, 35,
    41, 42, 43, 44, 45
  )
  val fu: Array[BigDecimal] = Array(
    11, 12, 13, 14, 15,
    21, 22, 23, 24, 25,
    31, 32, 33, 34, 35,
    41, 42, 43, 44, 45
  )
  val t = Interpolated2D(xAxis, yAxis, ft)
  val u = Interpolated2D(xAxis, yAxis, fu)
  val aliases = Map("t" -> t, "u" -> u)

  it should "multiple a table by a scalar" in {
    inside(XdfExpressionParser.from(aliases).parse("10*t")) { case Interpolated2D(x, y, z) =>
      z.toSeq shouldBe Seq(
        110, 120, 130, 140, 150,
        210, 220, 230, 240, 250,
        310, 320, 330, 340, 350,
        410, 420, 430, 440, 450
      )
    }
  }

  it should "evaluate a table expression" in {
    val expected = ft.zip(fu).map((t, u) => (BigDecimal("-0.9")*t + u)/2)
    inside(XdfExpressionParser.from(aliases).parse("(-0.9*t + u) / 2")) { case Interpolated2D(x, y, z) =>
      z.toSeq shouldBe expected.toSeq
    }
  }
  // format: on
}

object XdfExpressionParserTest {}
