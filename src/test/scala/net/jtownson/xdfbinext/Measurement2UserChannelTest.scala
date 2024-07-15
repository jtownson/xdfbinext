package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class Measurement2UserChannelTest extends AnyFlatSpec {
  val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  val a2l    = new Measurement2UserChannel(a2lUrl)

  "MeasurementToUserChannel" should "output a snippet for Nkw" in {
    // format: off
    val expected = """  <ActualValue ReqBlock="50802002" Size="2" DataA="1.0" DataB="0.0" Prefix="50" Units="1/min" RoundingDigits="3" signed="1">
                     |    <Text xml:lang="en">Nkw</Text>
                     |  </ActualValue>""".stripMargin
    // format: on
    val actual = a2l.measurement2UserChannel(_ == "Nkw").mkString
    actual shouldBe expected
  }

  it should "output a snipped for Dzwt_pf1" in {
    // format: off
    val expected = """  <ActualValue ReqBlock="51801f14" Size="2" DataA="0.1" DataB="0.0" Prefix="51" Units="°" RoundingDigits="1" signed="1">
                     |    <Text xml:lang="en">Dzwt_pf1</Text>
                     |  </ActualValue>""".stripMargin
    // format: on
    val actual = a2l.measurement2UserChannel(_ == "Dzwt_pf1").mkString
    actual shouldBe expected
  }

  it should "output a snipped for BMWtchctr_pct_Wg_uw" in {
    // format: off
    val expected = """  <ActualValue ReqBlock="50801b04" Size="2" DataA="0.00152587890625" DataB="0.0" Prefix="50" Units="%" RoundingDigits="3">
                     |    <Text xml:lang="en">BMWtchctr_pct_Wg_uw</Text>
                     |  </ActualValue>""".stripMargin
    // format: on
    val actual = a2l.measurement2UserChannel(_ == "BMWtchctr_pct_Wg_uw").mkString
    actual shouldBe expected
  }

  it should "output a user channels file" in {
    // format: off
    val expected = """<?xml version="1.0" standalone="yes"?>
                     |<ActualValues>
                     |  <ActualValue ReqBlock="50802002" Size="2" DataA="1.0" DataB="0.0" Prefix="50" Units="1/min" RoundingDigits="3" signed="1">
                     |    <Text xml:lang="en">Nkw</Text>
                     |  </ActualValue>
                     |</ActualValues>""".stripMargin
    // format: on
    val actual = a2l.measurement2UserChannels(_ == "Nkw").mkString
    actual shouldBe expected
  }

  it should "output multiple values for arrays" in {
    val actual = a2l.measurement2UserChannel(_ == "Zw_out")
    actual.length shouldBe 6
  }
}
