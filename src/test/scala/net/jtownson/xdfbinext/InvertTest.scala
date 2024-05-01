package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.Invert.tableInvertX
import net.jtownson.xdfbinext.InvertTest.max
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
    // format: off
    val xaxis = (0 to 10).map(BigDecimal(_)).toArray
    val yaxis = (0 to 2).map(BigDecimal(_)).toArray
    val values = Array[BigDecimal]( xs =
      0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
      0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100,
      0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
    )

    val (x2, y2, z2) = tableInvertX(xaxis, yaxis, values)
    val zt = Interpolated2D(x2, y2, z2)
    x2.toSeq shouldBe Seq(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    y2.toSeq shouldBe Seq(0, 1, 2)
    zt.atXY(10, 0) should equal (BigDecimal(Math.sqrt(10)) +- 0.1)
    zt.atXY(20, 0) should equal (BigDecimal(Math.sqrt(20)) +- 0.1)
    zt.atXY(30, 0) should equal (BigDecimal(Math.sqrt(30)) +- 0.1)
    zt.atXY(100, 0) should equal (BigDecimal(10) +- 0.1)
    zt.atRow(0) shouldBe zt.atRow(1)
    zt.atRow(1) shouldBe zt.atRow(2)
    // format: on
  }

  // format: off
  it should "invert part of a load to torque table" in {
    val load = Array(0, 12, 14, 18, 22).map(BigDecimal(_))
    val rpm = Array(500, 600, 800).map(BigDecimal(_))
    val loadRpmToTorque: Array[BigDecimal] = Array(
      0.0, 29.1, 34.0, 44.0, 55.0,
      0.0, 32.6, 38.0, 48.0, 63.0, 
      0.0, 36.0, 42.0, 52.0, 66.0
    )
    val (torque, rpm2, torqueRomToLoad) = tableInvertX(load, rpm, loadRpmToTorque)

    val (load3, rpm3, loadRpmToTorque2) = tableInvertX(torque, rpm2, torqueRomToLoad)

    loadRpmToTorque.zip(loadRpmToTorque2).foreach( (l, r) => r should equal (r +- max(r/10, 0.1)))

  }
  // format: on
}

object InvertTest {
  def max(l: BigDecimal, r: BigDecimal): BigDecimal = if (l > r) l else r
}
