package net.jtownson.xdfbinext.a2l

import net.jtownson.xdfbinext.a2l.FormulaExpressionInverse
import net.jtownson.xdfbinext.a2l.RatFun
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class FormulaExpressionInverseTest extends AnyFlatSpec {

  behavior of "RatFunFormula"

  it should "create a formula for int scaled quantities" in {
    FormulaExpressionInverse.toFormulaInv(0, 65536, 0, 0, 0, 1) shouldBe "x/65536"
  }

  it should "evaluate a formulate" in {
    RatFun(0, 65536, 0, 0, 0, 1)(1) shouldBe BigDecimal(1) / BigDecimal(65536)
  }

  it should "have an identity" in {
    RatFun.identity(0) shouldBe 0
    RatFun.identity(1) shouldBe 1
  }
}
