package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.EquationParser.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class EquationParserTest extends AnyFlatSpec:

  "Equation parser" should "calculate constant additions" in {
    parseConst("1+1") shouldBe 2
    parseConst("10+10") shouldBe 20
  }

  it should "calculate constant multiplications" in {
    parseConst("4250*0.1") shouldBe 425
  }

  it should "calculate abbreviated decimals" in {
    parseConst(".5*10") shouldBe 5
    parseConst("10*.5") shouldBe 5
  }

  it should "handle complex expressions" in {
    val (a, b, c, d) =
      (BigDecimal("3.14159265359"), BigDecimal("2.71828182846"), BigDecimal("1.4142135623"), BigDecimal("1.6180339887"))
    parseConst(s"$a+$b*$c/$d") shouldBe a + b * c / d
  }

  it should "parse a single variable algebraic expression" in {
    parseBigDecimalF1("X+1")(425) shouldBe 426
    parseBigDecimalF1("x+1.001")(425) shouldBe BigDecimal("426.001")
  }

  it should "enable short input but expand to BigDecimal" in {
    parseBigDecimalF1("x*0.1")(425) shouldBe BigDecimal("42.5")
  }
