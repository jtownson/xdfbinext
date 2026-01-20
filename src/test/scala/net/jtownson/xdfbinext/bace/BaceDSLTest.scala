//package net.jtownson.xdfbinext.bace
//
//import net.jtownson.xdfbinext.a2l.{A2LMeasurement, CurveType, MapType}
//import net.jtownson.xdfbinext.a2l.CurveType.NumberNumberTable1D
//import net.jtownson.xdfbinext.a2l.MapType.NumberNumberNumberTable2D
//import net.jtownson.xdfbinext.bace.BaceDSL.{given, *}
//import net.jtownson.xdfbinext.bace.BaceLib.ifThenElse
//import org.mockito.Mockito.{verify, when}
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers.*
//import org.scalatestplus.mockito.MockitoSugar.mock
//
//class BaceDSLTest extends AnyFlatSpec {
//  behavior of "BaceDSL"
//
//  it should "if/then/else should support BigDecimal inputs" in {
//    (BigDecimal(1), BigDecimal(Int.MaxValue), BigDecimal(Int.MinValue)) --> ifThenElse[
//      BigDecimal,
//      BigDecimal
//    ] shouldBe BigDecimal(Int.MaxValue)
//    (BigDecimal(0), BigDecimal(Int.MaxValue), BigDecimal(Int.MinValue)) --> ifThenElse[
//      BigDecimal,
//      BigDecimal
//    ] shouldBe BigDecimal(Int.MinValue)
//  }
//
//  it should "if/then/else should support String inputs" in {
//    ("true", "A", "B") --> ifThenElse[String, String] shouldBe "A"
//    ("false", "A", "B") --> ifThenElse[String, String] shouldBe "B"
//  }
//
//  it should "write one measurement to another" in {
//    val in = mock[A2LMeasurement]
//    val c  = BigDecimal(1)
//    when(in.read1).thenReturn(c)
//    val out = mock[A2LMeasurement]
//
//    in --> out
//
//    verify(out).write1(c)
//  }
//
//  it should "write a numeric constant to a measurement (directly)" in {
//    val c   = BigDecimal(1)
//    val out = mock[A2LMeasurement]
//
//    c --> out
//
//    verify(out).write1(c)
//  }
//
//  it should "write a constant to a curve" in {
//    val kl: CurveType[BigDecimal, BigDecimal] =
//      NumberNumberTable1D(Array[BigDecimal](1, 2, 3), Array[BigDecimal](1, 2, 3))
//    val in = BigDecimal(2)
//
//    val a = in --> kl
//
//    a shouldBe BigDecimal(2)
//  }
//
//  it should "write a curve output to a measurement" in {
//    val kl: CurveType[BigDecimal, BigDecimal] =
//      NumberNumberTable1D(Array[BigDecimal](1, 2, 3), Array[BigDecimal](2, 4, 6))
//    val in  = BigDecimal(2)
//    val out = mock[A2LMeasurement]
//
//    in --> kl --> out
//
//    verify(out).write1(BigDecimal(4))
//  }
//
//  it should "write a pair of constants to a map" in {
//    val kf =
//      NumberNumberNumberTable2D(
//        Array[BigDecimal](1, 2, 3),
//        Array[BigDecimal](1, 2, 3),
//        Array[BigDecimal](1, 2, 3, 1, 2, 3, 1, 2, 3)
//      )
//    val inX = BigDecimal(2)
//    val inY = BigDecimal(2)
//
//    val out = (inX, inY) --> kf
//
//    out shouldBe kf.atXY(2, 2)
//  }
//
//  it should "write a map output to a measurement" in {
//    val kf =
//      NumberNumberNumberTable2D(
//        Array[BigDecimal](1, 2, 3),
//        Array[BigDecimal](1, 2, 3),
//        Array[BigDecimal](1, 2, 3, 1, 2, 3, 1, 2, 3)
//      )
//    val inX  = BigDecimal(2)
//    val inY  = BigDecimal(2)
//    val zOut = mock[A2LMeasurement]
//
//    (inX, inY) --> kf --> zOut
//
//    verify(zOut).write1(kf.atXY(2, 2))
//  }
//}
