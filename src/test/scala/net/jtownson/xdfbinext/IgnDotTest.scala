package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2L2Dot.{GraphOptions, MeasurementRole}
import net.jtownson.xdfbinext.IgnDotTest.{ignPimBlock, ignoreCharacteristics, ignoreFns, ignoreMeasurements}
import org.scalatest.flatspec.AnyFlatSpec

import scala.jdk.CollectionConverters.*

class IgnDotTest extends AnyFlatSpec {

  val a2lUrl  = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  val a2l2Dot = new A2L2Dot(a2lUrl)

  it should "graph ign functions - high level flow" in {

    val valsToInclude = Set(
      "Nkw",
      "Tmot",
      "Tans",
      "Rfv_vns",
      "Rf_zwv",
      "Zw_out",
      "Drf_soll",
      "BMWign_ag_OutMv_sw",
      "BMWign_ag_OutCrtdXcy_sw",
      "F_dzw_dyn",
      "Zw_min_ext",
      "Zw_min",
      "F_dzwt_pf1",
      "F_dzwt_pf2",
      "BMWign_b_ag_KnkCtlOut_bo",
      "BMWign_dag_PclFil_sw",
      "Dzwt",
      "Dzwt_best_pf1",
      "Dzwt_best_pf2",
      "Dzwdyn",
      "Dzw_kr",
      "Zw_grund_kg",
      "Zw_soll",
      "BMWign_ag_MinNorm_sw"
    )

    val valsToIgnore = Set(
      "B_zylakt_igninj_enable"
    )

    val mapsToInclude = Set[String](
      "KF_ZW_PF1",
      "KF_ZW_PF2",
      "KF_ZW_S_PF1",
      "KF_ZW_S_PF2",
      "KF_ZW_UESP_PF1",
      "KF_ZW_UESP_PF2",
      "KF_DZWT_PF1",
      "KF_DZWT_PF2",
      "KF_DZWT_UESP_PF1",
      "KF_DZWT_UESP_PF2",
      "KF_FDZWT_PF1",
      "KF_FDZWT_PF2",
      "KF_ZWMIN_KMS",
      "KF_ZWMIN_WMS",
      "KF_DZW_DYN",
      "IKCtl_FacKnockDetThd0_GM"
    )

    val fnsToInclude = Set(
      "BMW_MOD_IgnBas_BestCoord_seg",
      "BMW_MOD_IgnBas_Norm_seg",
      "BMW_MOD_IgnBasDelt_Temp_1000ms",
      "BMW_MOD_IgnBasDelt_Temp_seg",
      "BMW_MOD_IgnBasDelt_Dyn_10ms",
      "BMW_MOD_IgnBas_KnkBnd_seg",
      "BMW_MOD_IgnMin_Cond_10ms",
      "BMW_MOD_IgnMin_Coord_seg",
      "BMW_MOD_IgnMin_100ms",
      "BMW_MOD_IgnMin_EngPron_seg",
      "BMW_MOD_IgnIf_Bas_seg",
      "BMW_MOD_IgnOut_IndCrtn_seg",
      "BMW_MOD_IgnOut_Out_seg",
      "BMW_SWC_KraNN_10ms"
//      "BMW_SWC_TqeBas_Int"
    )
    val fnPred = (s: String) => fnsToInclude.contains(s)

    val valsPred =
      (name: String, role: MeasurementRole) =>
        (role == MeasurementRole.InOut || valsToInclude.contains(name) || name.toLowerCase.startsWith(
          "dzw"
        )) && !valsToIgnore.contains(name)

    val mapsPred = (s: String) => mapsToInclude.contains(s)

    A2L2Dot.functionCentredGraphWith(
      "Ign_038_005_0",
      a2l2Dot,
      fnPred,
      mapsPred,
      valsPred,
      GraphOptions(false, false),
      "Ign_038_005_0-hlf"
    )
  }

  it should "graph ign functions" in {

    val additionalFns = Set(
      "BMW_SWC_TqeIgGraph_Int",
      "BMW_MOD_TqeBasIgSpRamp_seg",
      "BMW_MOD_TqeIgGraphZwo_seg",
      "BMW_MOD_TqeIgGraphZwoCth_seg",
      "BMW_SWC_KraNN_10ms",
      "KRKE"
    )

    val fnPred =
      (s: String) => a2l2Dot.parentFnPredicate("Ign_038_005_0")(s) || additionalFns.contains(s)

    val ignoreValsPred =
      (s: String, role: MeasurementRole) => role != MeasurementRole.Loc

    val ignoreMapsPred = (s: String) => !ignoreCharacteristics.contains(s)

    A2L2Dot.functionCentredGraphWith(
      "Ign_038_005_0",
      a2l2Dot,
      fnPred,
      ignoreMapsPred,
      ignoreValsPred,
      GraphOptions(false, false),
      "Ign_038_005_0"
    )
  }
}

object IgnDotTest {
  val ignPimBlock = Set(
    "BMW_MOD_IgnPim_SknkGlbRea_seg",
    "BMW_MOD_IgnPim_SknkCylRea_seg",
    "BMW_MOD_IgnPim_GlwDetRaw_seg",
    "BMW_MOD_IgnPim_GlwDetCrtn_seg",
    "BMW_MOD_IgnPim_CylOff_seg",
    "BMW_MOD_IgnPim_GlwCoord_seg",
    "BMW_MOD_IgnPim_AfrInjMpl_seg",
    "BMW_MOD_IgnPim_GlbRea_100ms",
    "BMW_MOD_IgnPim_GlbReaFis_100ms",
    "BMW_MOD_IgnPim_Mem_seg",
    "BMW_MOD_IgnPim_DrvgProf_100ms"
  )
  val ignoreFns = Set(
    "BMW_MOD_IgnPlugSoot_1000ms",
    "BMW_MOD_IgnPlugSoot_100ms",
    "BMW_MOD_IgnBas_Vp_seg",
    "BMW_MOD_IgnStrt_Seg",
    "BMW_MOD_IgnOut_StRamp_Seg",
    "BMW_MOD_IgnMin_Dfco_seg",
    "BMW_MOD_IgnMin_AftStr_seg",
    "BMW_MOD_IgnMin_LamCrtn_seg",
    "BMW_MOD_IgnIf_TaskCoord_seg"
  ) ++ ignPimBlock

  val ignoreMeasurements = Set(
    "BMWign_ag_MinBrngBndFiChg_sb",
    "F_dzwt_best_pf1",
    "BMWpim_b_GlbReaFisWrNv_bo",
    "Zw_min_kh_pf4",
    "Zwstat_pf1",
    "F_dynvz",
    "Zw_min_kh",
    "Bb_zwmin_ms",
    "Zw_pf2",
    "Dzw_wl_pf2",
    "BMWpim_b_GlwDetDiCylOff_bo",
    "BMWpim_ct_PrevnInttNv_uw",
    "Dzwmin_lakor",
    "Zw_min_kh_pf1",
    "Dzwt_best_pf1",
    "Zwstat_pf2",
    "Zw_min_kh_pf2",
    "BMWpim_fac_PrevnPby_uw",
    "BMWpim_ct_GlbReaRstAck_ub",
    "Koeff_a_la",
    "St_zwgrund1.B_zwdynakt",
    "La_bez",
    "Dzw_wl_pf1",
    "St_zwgrund1",
    "Dzw_la_bez",
    "Td_dynakt",
    "St_zwgrund1.B_zwsteig",
    "Tddrfsol",
    "St_zwgrund1.B_zwfall",
    "St_zwgrund1.B_trigout",
    "St_zwgrund1.B_rfsola",
    "Zw_min_bg_pf2",
    "Zw_min_bg_pf1",
    "Td_dynvz",
    "F_dzwt_best_pf2",
    "F_dynakt",
    "Td_dyndek",
    "BMWign_b_FiChgFirstCall_bo",
    "BMWpim_amp_GlwDet_uw",
    "B_Zylakt_Ign_enable",
    "Zw_pf1",
    "St_zwgrund1.B_zwdynvz",
    "Zw_min_kh_pf12",
    "B_zylakt_igninj_enable",
    "St_zwgrund1.B_trigin",
    "Zw_min_kh_pf3",
    "Zw_min_bg",
    "BMWpim_fac_GlwDetDyn_uw",
    "Zw_min_kh_pf34",
    "BMWpim_amp_GlwDetDyn_sw",
    "B_zylakt_igninj_calc",
    "BMWpim_fac_SknkGlbReaNv_uw",
    "Koeff_b_la"
  )

  val ignoreCharacteristics = Set(
    "KF_FZW_WL_PF1",
    "KF_KOEFF_DZWTANS_PF2",
    "KL_F_DZWKRAP",
    "BMWign_ag_OutMan_Ca",
    "KF_DZW_WL_PF2",
    "BMWign_cw_ag_OutMan_C",
    "KF_KOEFF_A_LA",
    "KF_ZWBEST_VP_PF1",
    "BMWpim_nr_GlwDetDiCycClu_C",
    "K_DZW_LA_KOR_MN",
    "BMWign_fac_dag_PclFilFull_T",
    "K_DZW_LA_KOR_MX",
    "KF_DZWT_BEST_PF1",
    "KF_ZW_BEST_BS_PF1",
    "KF_ZW_UESP_BEST_BS_PF2",
    "BMWpim_nr_GlwDetDiCycCylOff_C",
    "KL_DZWMIN_LAKOR_TQE",
    "KF_ZW_BEST_BS_PF2",
    "KF_FDZWT_BEST_PF2",
    "BMWign_ag_OutManTmp_C",
    "KF_KOEFF_DZWTANS_PF1",
    "BMWign_dag_PclFilFull_M",
    "S_ZWDYNRETRIG",
    "KF_DZWTANS_PF2",
    "S_DRFFLANKEIN",
    "KF_LA_BEZ",
    "BMWpim_nr_GlwDetDiCycSmaTq_C",
    "KF_KOEFF_B_LA",
    "BMWpim_nr_GlwDetDiCycWinChg_C",
    "BMWign_idx_ag_OutManTmp_C",
    "KF_FDZWT_BEST_PF1",
    "KF_DZWTANS_PF1",
    "KF_ZWBEST_VP_PF2",
    "KF_DZW_WL_PF1",
    "KF_DZWT_BEST_PF2",
    "KF_FZW_WL_PF2",
    "BMWign_nr_ag_OutManTmp_C",
    "BMWign_ag_OutMan_C",
    "KF_ZW_UESP_BEST_BS_PF1",
    "KF_FDZWMIN_LAKOR_TQE"
  )
}
