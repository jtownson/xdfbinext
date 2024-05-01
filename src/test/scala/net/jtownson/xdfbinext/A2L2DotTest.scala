package net.jtownson.xdfbinext

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.MutableGraph
import guru.nidi.graphviz.parse.Parser
import net.jtownson.xdfbinext.A2L2Dot.{functionCentredGraphWith, valueCentredGraphWith}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks.*

import java.io.File
import scala.util.Using

class A2L2DotTest extends AnyFlatSpec {

  behavior of "A2l2Dot"

  def withA2L(test: A2L2Dot => Any): Unit = {
    val a2lUrl  = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
    val a2l2Dot = new A2L2Dot(a2lUrl)
    test(a2l2Dot)
  }

  private val handGraphs = Table[String](
    "filename",
    "tch-pwr-ff.dot",
    "tch-pwr-p.dot",
    "tch-pwr-d.dot",
    "tch-pwr.dot",
    "wg-basc.dot",
    "ausy_turb.dot"
  )

  private val fnCentredGraphs = Table[String](
    "functionName",
    "BMW_MOD_TchSp_P_10ms",
    "BMW_MOD_AusyKat_10ms",
    "BMW_MOD_TchCtr_Pwr_10ms",
    "BMW_MOD_TchCtr_Pwr2Pos_10ms"
    //    "BMW_MOD_AusyKat_10ms",
//    "BMW_MOD_TchBas_P_10ms",
//    "BMW_MOD_TchBas_Misc_10ms",
//    "BMW_MOD_BlsRfMax_10ms",
//    "BMW_SWC_Ewg",
//    "BMW_MOD_TchCtr_Pwr_10ms",
//    "BMW_MOD_TchCtr_PosAdIp_10ms",
//    "BMW_MOD_TchOut_10ms",
//    "BMW_MOD_TqeLimStatMaxMdk_10ms",
//    "BMW_MOD_BsPost_200ms"
  )

  private val valueCentredGraphs = Table[String](
    "Object name",
    "BMWtchsp_fac_mf_CmprNorm_uw",
    "St_einh_md_sport",
    "BMWtchbas_p_Dif_sw",
    "BMWtchctr_pct_Wg",
    "BMW_MOD_BlsHub_HubevsollKF_10ms",
    "P_MDGI_HAXL",
    "BMWtqe_tqc_FlApplStgNorm_T",
    "Nkw_opt",
    "K_FRFMXBS_MN"
  )

  it should "render hand drawn graphs" in forAll(handGraphs) { filename =>
    A2L2DotTest.resourcesGraph(filename)
  }

  it should "render fn centred graphs" in forAll(fnCentredGraphs) { fnName =>
    withA2L(a2l => functionCentredGraphWith(a2l, _ => true, _ == fnName, s"$fnName.svg"))
  }

  it should "render value centred graphs" ignore forAll(valueCentredGraphs) { objName =>
    withA2L(a2l => valueCentredGraphWith(a2l, _ == objName, s"${objName}.svg"))
  }

  it should "render BMWtchco_b_Acv_bo" in {
    withA2L(a2l => valueCentredGraphWith(a2l, _ == "BMWtchsp_mf_Ex_uw", s"BMWtchsp_mf_Ex_uw.svg"))
  }

  it should "graph the wg pos" in {
    withA2L { a2l =>
      val vars = Set(
        "Rf_max_pldmax",
        "BMWtchsp_p_DifIco_uw",
        "Pld_soll_max_xeb",
        "BMWtchsp_rat_p_Cmpr_uw",
        "BMWtchsp_p_ReqDyn2_sw",
        "BMWtchsp_rf_ReqMax_uw",
        "BMWtchsp_rat_p_CmprLim_uw",
        "BMWtchsp_p_ReqDyn_sw",
        "BMWtchsp_p_Req_uw",
        "BMWtchsp_volf_Ico_uw",
        "BMWtchbas_p_BefCmpr_uw",
        "BMWtchtbc_t_BefCmpr_sw",
        "BMWtchdiag_b_rf_Lim_bo",
        "BMWtchsp_mf_CmprNorm_uw",
        "Vsllk",
        "B_atl_rfmaxred",
        "Pvld",
        "Tvldr",
        "Mssolatl_k",
        "BMWtchsp_volf_Ico_uw_",
        "BMWtchdiag_b_rf_Lim_bo_",
        "BMWtchbas_p_BefCmpr_uw_",
        "BMWtchtbc_t_BefCmpr_sw_",
        "BMWtchsp_mf_CmprNorm_uw_",
        "BMW_MOD_TchSp_P_10ms",
        "BMWtchsp_volf_Ico_uw",
        "BMWtchdiag_b_rf_Lim_bo",
        "Drft_uk",
        "Fupsrf_kor_f",
        "Nkw",
        "Pirg_kor_f",
        "BMWtchbas_p_BefCmpr_uw",
        "Rf_vlsaug_max",
        "BMWtchtbc_t_BefCmpr_sw",
        "BMWtchsp_mf_CmprNorm_uw",
        "Pld_soll_xeb",
        "BMWtchdiag_b_tq_Lim_bo",
        "BMWtchsp_rat_p_Cmpr_uw",
        "BMWtchsp_rf_ReqMax_uw",
        "BMWtchsp_p_Req_uw",
        "BMWtchsp_p_ReqDyn_sw",
        "BMWtchsp_rat_p_CmprLim_uw",
        "BMWtchsp_p_ReqDyn2_sw",
        "Pld_soll_max_xeb",
        "BMWtchsp_p_DifIco_uw",
        "Rf_max_pldmax",
        "BMWtchsp_swi_ClcOld_bo",
        "Mssolatl_k",
        "Tvldr",
        "Pvld",
        "B_atl_rfmaxred",
        "Vsllk",
        "BMWtchdiag_b_tq_Lim_bo",
        "Pumg",
        "Pld_soll_xeb",
        "BMWtchtbc_t_BefCmpr_sw",
        "Rf_vlsaug_max",
        "BMWtchbas_p_BefCmpr_uw",
        "Pirg_kor_f",
        "Nkw",
        "Fupsrf_kor_f",
        "Drft_uk",
        "BMWtchdiag_b_rf_Lim_bo",
        "B_abgasklappe",
        "BMWtchctr_rat_p_Trb_uw",
        "BMWtchco_b_ClcCtlr_bo",
        "BMWtchbas_b_FlGc_bo",
        "BMWtchsp_p_ReqDyn_sw",
        "Rf",
        "Tabg_mw",
        "Vsa_spri_i",
        "Nkw",
        "F_msturb_kor_inv",
        "BMWtchsp_mf_Ex_uw",
        "Pumg",
        "BMWtchctr_pct_WgBasc_uw",
        "BMWtchctr_rat_p_Trb_uw",
        "KL_AUSYKAT_AKZU",
        "KL_AUSYKAT_AKAUF",
        "KL_FPKATREL_PUMG"
      )

      val fns =
        Set(
          "BMW_MOD_TchCtr_Pwr2Pos_10ms",
          "BMW_MOD_AusyTurb_Seg",
          "BMW_MOD_AusyKat_10ms",
          "BMW_MOD_TchSp_P_10ms",
          "BMW_MOD_TchSp_Volf_10ms"
        )

      withA2L(a2l => functionCentredGraphWith(a2l, vars.contains, fns.contains, s"pwr2-pos.svg"))
    }
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
