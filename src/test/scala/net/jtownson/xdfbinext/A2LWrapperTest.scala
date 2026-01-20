package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2LWrapperTest.withA2L
import net.jtownson.xdfbinext.a2l.CharacteristicSummary.{CurveSummary, MapSummary, ValueSummary}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class A2LWrapperTest extends AnyFlatSpec {

  behavior of "A2LWrapper"

  it should "Get functions for a characteristic" in withA2L { a2l =>
    a2l.characteristicUsage("KL_AUSY_TURB") shouldBe Set("BMW_MOD_AusyTurb_Seg")
    a2l.characteristicUsage("KF_AUSY_TURB") shouldBe Set("BMW_MOD_AusyTurb_Seg", "BMW_MOD_TchCtr_Pwr2Pos_10ms")
    a2l.characteristicUsage("BMWtchsp_p_DifIco_T") shouldBe Set("BMW_MOD_TchSp_P_10ms")
  }

  it should "generate a summary for a value" in withA2L { a2l =>
    a2l.getSummary("K_FRFMXBS_MN") shouldBe ValueSummary(
      "K_FRFMXBS_MN",
      "Factor for load reduction in component protection mode",
      Set("BMW_MOD_BsPost_200ms"),
      "-"
    )
  }

  it should "generate a summary for a curve" in withA2L { a2l =>
    a2l.getSummary("KL_AUSY_TURB") shouldBe CurveSummary(
      "KL_AUSY_TURB",
      "Flow characteristic curve turbine, reduced mass flow (unit kg/s * root [°K] / kPa)",
      Set("BMW_MOD_AusyTurb_Seg"),
      "-",
      "-"
    )
  }

  it should "generate a summary for a map" in withA2L { a2l =>
    a2l.getSummary("BMWtchsp_rat_p_CmprMax_M") shouldBe MapSummary(
      "BMWtchsp_rat_p_CmprMax_M",
      "KF_FPLDMAX",
      Set("BMW_MOD_TchSp_P_10ms"),
      "kg/h",
      "°C",
      "-"
    )
  }

  it should "generate summaries for all objects" ignore withA2L { a2l =>
    a2l.characteristics.foreach { (n, c) =>
//      if (c.getType != CharacteristicType.ASCII)
      println(a2l.getSummary(n))
    }
  }

  it should "get the memory segment for an address" in withA2L { a2l =>
    a2l.segmentForAddress(0x51802740) shouldBe 0x51800000
    a2l.segmentForAddress(0x50801af0) shouldBe 0x50800000
  }

  it should "get a format for St_kr.B_krdws" in withA2L { a2l =>
    a2l.getFormat(a2l.measurements("St_kr.B_krdws")) shouldBe (6, 3)
  }

  it should "read this table with absolute axis" in withA2L { a2l =>
    a2l.characteristics
  }

  it should "correctly determine the type of a numeric constant" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("BMWtchctr_fac_FilLimPctl_C")
      .withDefaultArgs(fNumber = () => 1)

    applied shouldBe 1
  }

  it should "correctly determine the type of a string constant" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("B_VMDEAK397_V")
      .withDefaultArgs(fString = () => 1)

    applied shouldBe 1
  }

  it should "correctly determine the type of a normal numeric curve" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("BMWtchco_fac_FadePl_T")
      .withDefaultArgs(fNumberNumber = () => 1)

    applied shouldBe 1
  }

  it should "correctly determine the type of a numeric curve with absolute axis" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("MfVDTypC_KLVZMSVUB")
      .withDefaultArgs(fNumberNumber = () => 1)

    applied shouldBe 1
  }

  it should "correctly determine the type of KL_TD_HBA_F (a curve mapping string -> numbers)" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("KL_TD_HBA_F")
      .withDefaultArgs(fStringNumber = () => 1)

    applied shouldBe 1
  }

  it should "correctly determine the type of a valblk" in withA2L { a2l =>
    val applied = a2l
      .saneCharacteristicTypeFold[Int]("BMWchm_fac_TqRsv_Ca")
      .withDefaultArgs(fNumberArr = () => 1)

    applied shouldBe 1
  }

  it should "get the formula for all measurements" in withA2L { a2l =>
    a2l.measurementsOrdered.foreach { m =>
      val compuMethod = a2l.getFormula(m)

      val printit: () => Unit = () => println(s"${m.getName}, ${compuMethod}")

      A2LWrapper.compuMethodTypeFold(
        compuMethod,
        fVtab = printit,
        fTab = () => (),
        fRatFun = () => ()
      )

//      println(s"${m.getName}, ${a2l.getFormula(m)}")
//      noException shouldBe thrownBy(methodType)
    }
  }

  it should "correctly determine the type of a measurement" in withA2L { a2l =>
    a2l.measurementTypeFold("BMWFl_st_PTankMod_ub", fString = () => 0, fNumber = () => 1) shouldBe 1
    a2l.measurementTypeFold("t_swi_el_nvld", fString = () => 0, fNumber = () => 1) shouldBe 1
    a2l.measurementTypeFold("BMWlpsc_st_PTankModSub_C", fString = () => 0, fNumber = () => 1) shouldBe 1
    a2l.measurementTypeFold("dvest_flgClr", fString = () => 0, fNumber = () => 1) shouldBe 1
  }
}

object A2LWrapperTest {

  val a2lUrl          = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  lazy val a2lWrapper = A2LWrapper(a2lUrl)

  def withA2L(test: A2LWrapper => Any): Unit = {
    test(a2lWrapper)
  }
}
