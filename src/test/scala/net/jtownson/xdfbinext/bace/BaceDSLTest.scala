package net.jtownson.xdfbinext.bace

import net.jtownson.xdfbinext.a2l.{CurveType, MapType}
import net.jtownson.xdfbinext.a2l.CurveType.NumberNumberTable1D
import net.jtownson.xdfbinext.a2l.MapType.NumberNumberNumberTable2D
import net.jtownson.xdfbinext.bace.BaceDSL.{-->, MeasurementType}
import net.jtownson.xdfbinext.bace.BaceDSL.MeasurementType.{InMeasurement, OutMeasurement}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class BaceDSLTest extends AnyFlatSpec {
  behavior of "BaceDSL"

  it should "write one measurement to another" in {
    val in  = InMeasurement[BigDecimal](1)
    val out = OutMeasurement[BigDecimal]()

    in --> out

    out.read shouldBe BigDecimal(1)
  }

  it should "write a numeric constant to a measurement (directly)" in {
    val c   = BigDecimal(1)
    val out = OutMeasurement[BigDecimal]()

    c --> out

    out.read shouldBe BigDecimal(1)
  }

  it should "write a string constant to a measurement (directly)" in {
    val s   = "hello"
    val out = OutMeasurement[String]()

    s --> out

    out.read shouldBe "hello"
  }

  it should "write a constant to a curve" in {
    val kl: CurveType[BigDecimal, BigDecimal] =
      NumberNumberTable1D(Array[BigDecimal](1, 2, 3), Array[BigDecimal](1, 2, 3))
    val in  = BigDecimal(2)
    val out = OutMeasurement[BigDecimal]()

    in --> kl --> out

    out.read shouldBe BigDecimal(2)
  }

  it should "write a pair of constants to a map" in {
    val kf =
      NumberNumberNumberTable2D(
        Array[BigDecimal](1, 2, 3),
        Array[BigDecimal](1, 2, 3),
        Array[BigDecimal](1, 2, 3, 1, 2, 3, 1, 2, 3)
      )
    val inX  = BigDecimal(2)
    val inY  = BigDecimal(2)
    val zOut = OutMeasurement[BigDecimal]()

    (inX, inY) --> kf --> zOut

    zOut.read shouldBe kf.atXY(2, 2)
  }

  it should "write a curve read to a measurement" in {

    val kl: CurveType[BigDecimal, BigDecimal] =
      NumberNumberTable1D(Array[BigDecimal](1, 2, 3), Array[BigDecimal](1, 2, 3))
    val in  = InMeasurement[BigDecimal](2)
    val out = OutMeasurement[BigDecimal]()

    in --> kl --> out

    out.read shouldBe BigDecimal(2)
  }

  it should "write a map read to a measurement" in {
    val kf =
      NumberNumberNumberTable2D(
        Array[BigDecimal](1, 2, 3),
        Array[BigDecimal](1, 2, 3),
        Array[BigDecimal](1, 2, 3, 1, 2, 3, 1, 2, 3)
      )
    val inX  = InMeasurement[BigDecimal](2)
    val inY  = InMeasurement[BigDecimal](2)
    val zOut = OutMeasurement[BigDecimal]()

    (inX, inY) --> kf --> zOut

    zOut.read shouldBe kf.atXY(2, 2)
  }
}
