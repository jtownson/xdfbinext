package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
class BmwTchDescriptionsTest extends AnyFlatSpec {
  behavior of "BmwTchDescriptions"

  it should "load a description" in {
    BmwTchDescriptions.table("BMWtchad_fac_Ip_C") shouldBe "Integration factor"
  }

  it should "load a description containing a comma" in {
    BmwTchDescriptions.table("BMWtchco_cw_FadeDynSpt_C") shouldBe "Bit-coded, at which st_Opm_ub is switched to the sport blending factors."
  }
}
