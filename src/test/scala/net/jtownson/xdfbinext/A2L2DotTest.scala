package net.jtownson.xdfbinext

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.MutableGraph
import guru.nidi.graphviz.parse.Parser
import net.jtownson.xdfbinext.A2L2Dot.{GraphOptions, functionCentredGraphWith, valueCentredGraphWith}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks.*

import java.io.File
import java.util.Base64
import scala.util.Using

class A2L2DotTest extends AnyFlatSpec {

  behavior of "A2l2Dot"

  private val a2lUrl  = getClass.getResource("/DME86S0_F4C9G576B.a2l").toURI.toURL
  private val a2l2Dot = new A2L2Dot(a2lUrl)

  def withA2L(test: A2L2Dot => Any): Unit = {
    test(a2l2Dot)
  }

  private val handGraphs = Table[String](
    "filename",
    "B58FR/test.dot"
//    "B58FR/BMW_MOD_IgnOut_kra_10ms.dot"
//    "BMW_MOD_IgnBasDelt_Msc_10ms.dot",
//    "tchdiag_pressure.dot",
//    "tchsp_p_Req_uw.dot",
//    "tch-pwr-pos_mf_trb.dot",
//    "tch-pwr-ff.dot",
//    "tch-pwr-p.dot",
//    "tch-pwr-d.dot",
//    "tch-pwr.dot",
//    "wg-basc.dot",
//    "ausy_turb.dot"
  )

  private val fnCentredGraphs = Table[String](
    "functionName",
//    "NMAXS"

    "BMW_MOD_Mafw_Pedal",
    "BMW_MOD_Mafw_Wunsch"
//    "P_MDGK_EVMKO_S_BGRZ"
//    "P_MDGS_10ms",
//    "BMW_MOD_Mafw_MdMax"
//    "MoFDrDem_Co"
//    "BMW_MOD_AsInFahrsit_100ms"
//    "EngDa_PwrEng"
//    "BMW_MOD_InjMon_TqLim_10ms",
//    "P_MDGMK_10ms",
//    "P_MDGMK_Gc",
//    "BMW_MOD_TqeLimInfo_10ms",
//    "BMW_MOD_TqeLimStatMaxExt_10ms"
//    "PFlt_InpSel",
//    "BMW_MOD_BlsDk_10ms",
//    "BMW_MOD_Bls_Pssol_10ms",
//    "BMW_MOD_ExtEngEb_200ms"
//    "P_MDGKSU_10ms"
//    "BMW_MOD_BsSollwerte_200ms",
//    "BMW_MOD_BsAtl_200ms",
//    "BMW_MOD_BsPost_200ms",
//    "BMW_MOD_BlsRfMax_10ms",
//    "BMW_MOD_BlsRfSoll_10ms",
//    "BMW_MOD_GpfDiag_Lim_100ms"
//    "BMW_MOD_TmcFett_200ms",
//    "BMW_MOD_CtbFl_seg"
//    "BMW_SWC_LoPSpdDiag",
//    "BMW_SWC_LoPSpdCtl",
//    "DKVBDE"
//    "BMW_MOD_InjSplt_seg"
//    "BMW_SWC_LamCo_Int",
//    "BMW_SWC_LamSp_Int",
//    "BMW_MOD_LamCo_3WHL_10ms"
//    "BMW_MOD_TqeBasIgSpRamp_seg",
//    "BMW_SWC_TqeBas_Int",
//    "BMW_MOD_IgnBas_BestCoord_seg"
//    "BMW_MOD_IgnIf_Rf_seg",
//    "BMW_MOD_IgnMin_LamCrtn_seg",
//    "BMW_MOD_IgnBasDelt_Msc_10ms",
//    "BMW_MOD_IgnOut_Out_seg",
//    "BMW_MOD_IgnOut_IndCrtn_seg"
//    "BMW_MOD_IgnBas_Norm_seg",
//    "BMW_MOD_IgnIf_Bas_seg",
//    "BMW_SWC_IgnIf_Int",
//    "BMW_MOD_IgnBas_KnkBnd_seg",
//    "BMW_MOD_IgnBasDelt_Temp_seg",
//    "BMW_MOD_IgnBasDelt_Msc_10ms",
//    "BMW_SWC_IgnBas_Int",
//    "BMW_SWC_KraNN_10ms",
//    "KRKE",
//    "KRREG",
//    "BMW_MOD_TqeIgGraphZwo_seg",
//    "BMW_MOD_IgnOut_Out_seg"
//    "BMW_MOD_AusyKat_10ms",
//    "P_MDGMK_STMDINFO_10ms",
//    "BMW_MOD_TchCo_Acv_10ms",
//    "P_MDGK_EVMKO_S_BGRZ",
//    "BMW_MOD_TchCtr_PwrFade_10ms",
//    "BMW_MOD_TchSp_P_10ms",
//    "BMW_MOD_TchSp_Volf_10ms",
//    "BMW_MOD_AusyKat_10ms",
//    "BMW_MOD_TchCtr_Pwr_10ms",
//    "BMW_MOD_TchCtr_Pwr2Pos_10ms"
//    "BMW_MOD_AusyKat_10ms",
//    "BMW_MOD_TchBas_P_10ms",
//    "BMW_MOD_TchBas_Misc_10ms"
//    "BMW_MOD_BlsRfMax_10ms",
//    "BMW_SWC_Ewg",
//    "BMW_MOD_TchCtr_Pwr_10ms",
//    "BMW_MOD_TchCtr_PosAdIp_10ms"
//    "BMW_MOD_TchOut_10ms",
//    "BMW_MOD_TqeLimStatMaxMdk_10ms",
//    "BMW_MOD_BsPost_200ms"
  )

  private val valueCentredGraphs = Table[String](
    "Object name",
//    "Mshfm_kor"
    "KL_MD_K_MAX_VL"
//    "Mdk_bst"
//    "MoFTrqPtd_tqCluMax_C"
//    "EngDa_tqInnrMax_C"
//    "KF_FTRANSVL"
//    "FuPHi_FlmMaxpl_T",
//    "K_KSU_MDEINGRIFF_MX"
//    "Frfmxbsld",
//    "BMWls_rf_MaxEgT",
//    "Tatlsoll",
//    "BMWext_t_DeltCptPronInCat_T",
//    "BMWls_rf_MaxEgT_M",
//    "La_vst_fett"
//    "Dzw_annm",
//    "K_TKW_MX"
//    "Zw_opt",
//    "Zw_soll",
//    "KF_ZWO_KHK_PF1",
//    "B_zw_dynman_vstg"
//    "KF_ZWOPT_PF1",
//    "Dzw_la_kor",
//    "KF_DZW_DYN",
//    "F_dzwt_pf1"
//    "KF_ZW_PF1",
//    "KF_ZWOPT_PF1",
//    "Zwstat",
//    "Dzwt",
//    "KF_LA_BEZ",
//    "Zw_grund_kg",
//    "Zwstat_best",
//    "Dzw_krann",
//    "KF_DZWKS",
//    "IKCtl_FacKnockDetThd0_GM",
//    "B_zw_dynman_vstg",
//    "K_ANZAHL_ZYLINDER"
//    "Pkat"
//    "Stat_mdinfo_s_tqc_tmp"
//    "Tabg_mw"
//    "K_I_GANG_AT"
//    "BMWtchbas_b_Noise_bo"
//    "BMWtchsp_fac_mf_CmprNorm_uw",
//    "St_einh_md_sport",
//    "BMWtchbas_p_Dif_sw",
//    "BMWtchctr_pct_Wg",
//    "BMW_MOD_BlsHub_HubevsollKF_10ms",
//    "P_MDGI_HAXL",
//    "BMWtqe_tqc_FlApplStgNorm_T",
//    "Nkw_opt",
//    "K_FRFMXBS_MN"
  )

  it should "render hand drawn graphs" in forAll(handGraphs) { filename =>
    A2L2DotTest.resourcesGraph(filename)
  }

  it should "render fn centred graphs" in forAll(fnCentredGraphs) { fnName =>
    withA2L(a2l =>
      functionCentredGraphWith(fnName, a2l, _ == fnName, _ => true, (_, _) => true, GraphOptions(true, true), fnName)
    )
  }

  it should "render value centred graphs" in forAll(valueCentredGraphs) { objName =>
    withA2L(a2l => valueCentredGraphWith(objName, a2l, _ == objName, objName))
  }

  it should "render some value" ignore {
    withA2L(a2l => valueCentredGraphWith("???", a2l, _ == "???", s"???.svg"))
  }

  it should "render TQE" in {
    val fnPred: String => Boolean = s => {
      s.contains("TqeEng") && s != "BMW_MOD_TqeEngStOut_100ms"
    }
    functionCentredGraphWith(
      graphName = "TQE",
      a2l2Dot = a2l2Dot,
      fnPredicate = fnPred,
      characteristicPredicate = _ => true,
      measurementPredicate = (_, _) => true,
      graphOptions = GraphOptions(false, false),
      filename = "tqe"
    )
  }

  it should "render comp prot" in {
    val fnPred: String => Boolean = s =>
      Set("BMW_MOD_BsSollwerte_200ms", "BMW_MOD_BsAtl_200ms", "BMW_MOD_BsPost_200ms", "BMW_MOD_BlsRfMax_10ms").contains(
        s
      )

    functionCentredGraphWith(
      "BMW_MOD_Bs",
      a2l2Dot,
      fnPred,
      _ => true,
      (_, _) => true,
      GraphOptions(false, true),
      "BMW_MOD_Bs"
    )
  }

}

object A2L2DotTest {

  def resourcesGraph(filename: String): Unit = {
    Using.resource(classOf[A2L2DotTest].getResourceAsStream(s"/$filename")) { dot =>
      val g: MutableGraph = new Parser().read(dot)
      Graphviz.fromGraph(g).render(Format.SVG).toFile(new File(s"$filename.svg"))
    }
  }
}
