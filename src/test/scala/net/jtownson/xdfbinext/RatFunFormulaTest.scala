package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class RatFunFormulaTest extends AnyFlatSpec {

  behavior of "RatFunFormula"

  it should "create a formula for int scaled quantities" in {
    RatFunFormula.toFormulaInv(0, 65536, 0, 0, 0, 1) shouldBe "x/65536"
  }
}
