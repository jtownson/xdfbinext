package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.CharacteristicType
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
      if (c.getType != CharacteristicType.ASCII)
        println(a2l.getSummary(n))
    }
  }
}

object A2LWrapperTest {

  private val tchCharacteristics = List(
    "BMWtchbas_p_DifAmbBefCmprFlp_T",
    "BMWtchbas_p_DifAmbBefCmpr_T",
    "BMWtchbas_fac_FilPBefThrDyn_C",
    "BMWtchbas_rat_p_TcBascSpt_T",
    "BMWtchbas_rat_p_TcBascEco_T",
    "BMWtchbas_rat_p_TcBascRoofOp_T",
    "BMWtchbas_rat_p_TcBasc_T",
    "BMWtchbas_fac_TcBascTCrtn_T",
    "BMWtchbas_fac_FilPDif_T",
    "BMWtchbas_fac_FilPDifCrtn_T",
    "BMWtchbas_nr_PDifGraAvg_C",
    "BMWtchbas_fac_n_FilDyn_C",
    "BMWtchbas_nr_n_AvgDyn2_C",
    "BMWtchbas_cw_Noise_C",
    "BMWtchbas_n_NoiseMin_C",
    "BMWtchbas_mf_NoiseMin_C",
    "BMWtchad_ti_DlyFlGcEna_C",
    "BMWtchbas_pct_AccrFlGcMin_T",
    "BMWtchbas_rat_p_BefThrFlGc_C",
    "BMWtchbas_pct_AccrGraFlGcMax_C",
    "BMWtchbas_ti_DlyAccr0FlGcDi_C",
    "BMWtchbas_ti_DlyFlGcCluDi_C",
    "BMWtchbas_ti_DlyFlGcPucDi_C",
    "BMWtchbas_ti_DlyFlGcMax_C",
    "BMWtchbas_ti_DlyFlGcHld_C",
    "BMWtchbas_cw_RaceStr_C",
    "BMWtchbov_b_AcvnMan_C",
    "BMWtchbov_swi_Man_C",
    "BMWtchbov_rat_p_InAmb_C",
    "BMWtchbov_nr_PDifPrdn_C",
    "BMWtchbov_p_DifAcvn_M",
    "BMWtchbov_rat_FiGraAcvn_T",
    "BMWtchbov_ti_DlyFiGraAcvn_C",
    "BMWtchbov_rat_FiGraDeac_C",
    "BMWtchbov_p_DifDeac_C",
    "BMWtchbov_cw_1_C",
    "BMWtchbov_ti_AcvnMax_C",
    "BMWtchbov_ti_AftStrtEndEna_C",
    "BMWtchbov_fac_mf_ThrThrMin_C",
    "BMWtchbov_fac_mf_ThrMin_C",
    "BMWtchbov_fac_mf_VvtMin_C",
    "BMWtchbov_mf_PmpLimIvs_T",
    "BMWtchco_ti_DlyAccrSpt_C",
    "BMWtchco_rat_gra_AccrSpt_C",
    "BMWtchco_rat_AccrSpt_C",
    "BMWtchco_cw_1_C",
    "BMWtchco_p_HysAcv_C",
    "BMWtchco_ti_DlyCtlrOff_C",
    "BMWtchco_fac_FadePl_T",
    "BMWtchco_swi_ClcPl_C",
    "BMWtchco_swi_ClcCtlr_C",
    "BMWtchco_swi_ClcPl_C",
    "BMWtchsp_fac_mf_FilCmprNorm_T",
    "BMWtchsp_fac_SqrtTBefTc_T",
    "BMWtchsp_fac_FadePBefTc_C",
    "BMWtchsp_swi_ClcOld_bo",
    "BMWtchsp_p_DifIco_T",
    "BMWtchsp_rat_p_CmprPmp_T",
    "BMWtchsp_rat_p_CmprMax_M",
    "BMWtchsp_fac_FilPRatCmpr_T",
    "BMWtchsp_rf_Liho_T",
    "BMWtchsp_p_OfsMax_C",
    "BMWtchsp_cw_1_C",
    "BMWtchsp_fac_FilPDyn_C",
    "BMWtchsp_nr_AvgPDyn2_C",
    "BMWtchctr_pwr_Pctl_M",
    "BMWtchctr_fac_FadeDyn_M",
    "BMWtchctr_swi_IpPwrAcvn_C",
    "BMWtchctr_pwr_Dp_M",
    "BMWtchctr_fac_Dp_T",
    "BMWtchctr_fac_DpDyn_T",
    "BMWtchctr_pwr_IpMax_C",
    "BMWtchctr_pwr_IpMin_C",
    "BMWtchctr_rat_WgIpStopMax_C",
    "BMWtchctr_fac_Ip_T",
    "BMWtchctr_pwr_CmprGra_M",
    "BMWtchctr_p_DifCrtnPp_M",
    "BMWtchctr_p_DifCrtnPpDyn_M",
    "BMWtchctr_fac_FilLimPctl_C",
    "BMWtchctr_fac_pwr_CompLimPctl_T",
    "BMWtchctr_p_ReqDynLimPctl_C",
    "BMWtchctr_rat_p_ComprLimPctl_T",
    "BMWtchctr_swi_ClcPwrWgPosn_C",
    "BMWtchctr_fac_FilRatPTrb_C",
    "KF_AUSY_TURB",
    "BMWtchctr_fac_TrbEffIvs_T",
    "BMWtchctr_fac_TrbEffPlsCrtn_M",
    "BMWtchctr_fac_TrbExp_T",
    "BMWtchctr_cpp_ExGas_T",
    "BMWtchctr_t_ExGasMdl_M",
    "BMWtchctr_fac_FadeTExGas_M",
    "BMWtchctr_fac_gra_FadeTExGas_C",
    "BMWtchctr_fac_FadeTExGas_C",
    "BMWtchctr_pct_WgBasc_M",
    "BMWtchctr_ti_DlyPl_T",
    "BMWtchctr_fac_FilRatWg_T",
    "BMWtchctr_fac_FilPctWgDyn_C",
    "BMWtchctr_pct_WgDyn_C",
    "BMWtchctr_pct_WgDeltSpdDyn_M",
    "BMWtchctr_pct_WgFlGc_T",
    "BMWtchctr_pct_WgDeltRaceStr_C",
    "BMWtchctr_pct_WgCthIdl_C",
    "BMWtchctr_pct_WgNoise_C",
    "BMWtchctr_pct_WgDeltPDyn_M",
    "BMWtchctr_fac_pct_WgDeltPDyn_M",
    "BMWtchctr_pct_WgClsdMin_C",
    "BMWtchctr_ti_DlyWgClsd_T",
    "BMWtchctr_pct_WgPl_M",
    "BMWtchctr_pct_WgPlSpt_M",
    "BMWtchctr_pct_WgPlEco_M",
    "BMWtchctr_pct_WgPlRoofOp_M",
    "BMWtchctr_fac_FadeAdpMax_T",
    "BMWtchctr_fac_FadeAdp_T",
    "BMWtchctr_pct_WgIpMan_C",
    "BMWtchctr_swi_WgIpMan_C",
    "BMWtchctr_swi_IpRstMan_C",
    "BMWtchctr_p_DifIpRstMax_C",
    "BMWtchctr_pct_WgIpRstMin_C",
    "BMWtchctr_p_DifIpRstMin_C",
    "BMWtchctr_pct_WgIpRstMax_C",
    "BMWtchctr_n_DynIpRstMax_C",
    "BMWtchctr_n_DynIpRstMin_C",
    "BMWtchctr_p_DynIpRstMax_C",
    "BMWtchctr_p_DynIpRstMin_C",
    "BMWtchctr_pct_WgPosnIpStopMax_C",
    "BMWtchctr_pct_WgSpIpStopMax_C",
    "BMWtchctr_pct_WgPosnIpStopMin_C",
    "BMWtchctr_pct_WgIpLimMax_T",
    "BMWtchctr_pct_WgIpLimMin_T",
    "BMWtchctr_fac_IpPctWg_M",
    "BMWtchdiag_swi_Rst_C",
    "BMWtchdiag_ti_DlyHiLo_T",
    "BMWtchdiag_pct_WgPHi_M",
    "BMWtchdiag_pct_WgPLo_M",
    "BMWtchdiag_n_EngMax_T",
    "BMWtchdiag_n_EngMin_C",
    "BMWtchdiag_rat_p_CmprMax_T",
    "BMWtchdiag_rf_SpMin_C",
    "BMWtchdiag_rat_p_CmprMin_M",
    "BMWtchdiag_ti_DlyDiagCmpl_C"
  )

  val a2lUrl          = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  lazy val a2lWrapper = A2LWrapper(a2lUrl)

  def withA2L(test: A2LWrapper => Any): Unit = {
    test(a2lWrapper)
  }
}
