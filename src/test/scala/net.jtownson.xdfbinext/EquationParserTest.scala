package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.EquationParser.{
  parseBigDecimal,
  parseBigDecimalF1,
  parseInt,
  parseShort,
  parseShortF1
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class EquationParserTest extends AnyFlatSpec:

  "Equation parser" should "add short" in {
    parseShort("1+1") shouldBe 2
    parseShort("10+10") shouldBe 20
  }

  it should "multiply short" in {
    parseShort("4250*0.1") shouldBe 425
    parseShortF1("X*0.1")(4250) shouldBe 425
  }

  it should "handle complex short expressions" in forAll {
    (a: Short, b: Short, c: Short, d: Short) =>
      parseShort(s"$a+$b*$c/$d") shouldBe (BigDecimal(a) + BigDecimal(b) * BigDecimal(
        c
      ) / BigDecimal(d)).toShort
  }

  it should "parse an additive integer expression" in {
    parseInt("1+1") shouldBe 2
    parseInt("10+10") shouldBe 20
  }

  it should "parse an additive big decimal expression" in {
    parseBigDecimal("3.14*3.14") shouldBe BigDecimal("9.8596")
  }

  it should "parse a single variable algebraic expression" in {
    parseBigDecimalF1("X+1")(425) shouldBe BigDecimal(426)
    parseBigDecimalF1("x+1.001")(425) shouldBe BigDecimal("426.001")
  }
