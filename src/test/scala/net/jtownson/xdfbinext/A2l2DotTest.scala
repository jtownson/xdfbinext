package net.jtownson.xdfbinext

import guru.nidi.graphviz.engine.Format
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.jdk.CollectionConverters.*
import guru.nidi.graphviz.engine.Graphviz
import net.jtownson.xdfbinext.A2l2DotTest.{functionCentredGraphWith, valueCentredGraphWith}

class A2l2DotTest extends AnyFlatSpec {

  behavior of "A2l2Dot"

  val a2lUrl  = getClass.getResource("/DME861_R1C9J9E3B.a2l").toURI.toURL
  val a2l2Dot = new A2l2Dot(a2lUrl)

//  it should "print node types" in {
//    a2l2Dot.asCommentCsv(name => name.toLowerCase.contains("ewg"))
//  }

  it should "create BMW_SWC_Ewg graph" in {
    val namePredicate: String => Boolean = s => s != "BMWewgco_st_Opm_ub"
    val fnPredicate: String => Boolean   = s => s.startsWith("BMW_SWC_Ewg")
    functionCentredGraphWith(a2l2Dot, namePredicate, fnPredicate, "BMW_SWC_Ewg.svg")
  }

  it should "create BMW_MOD_TchCtr_Pwr_10ms graph" in {
    val namePredicate: String => Boolean = s => true
    val fnPredicate: String => Boolean   = s => s == "BMW_MOD_TchCtr_Pwr_10ms"
    functionCentredGraphWith(a2l2Dot, namePredicate, fnPredicate, "TchCtr_Pwr_10ms.svg")
  }

  it should "create a graph for the ATL controller" in {
    val namePredicate: String => Boolean = s => true
    val fnPredicate: String => Boolean   = s => s == "BMW_MOD_TchCtr_PosAdIp_10ms"
    functionCentredGraphWith(a2l2Dot, namePredicate, fnPredicate, "TchCtr_PosAdIp_10ms.svg")
  }

  it should "create a graph with pwr atl controllers" in {
    val namePredicate: String => Boolean = s => true
    val fnPredicate: String => Boolean   = s => s == "BMW_MOD_TchCtr_Pwr_10ms" || s == "BMW_MOD_TchCtr_PosAdIp_10ms"
    functionCentredGraphWith(a2l2Dot, namePredicate, fnPredicate, "ATL_Pwr_10ms.svg")
  }

  it should "graph fn BMW_MOD_TchOut_10ms" in {
    val namePredicate: String => Boolean = s => true
    val fnPredicate: String => Boolean   = s => s == "BMW_MOD_TchOut_10ms"
    functionCentredGraphWith(a2l2Dot, namePredicate, fnPredicate, "TchOut_10ms.svg")

  }

  it should "create a graph centred on BMWtchbas_p_Dif_sw" in {
    val namePredicate: String => Boolean = s => s == "BMWtchbas_p_Dif_sw"
    valueCentredGraphWith(a2l2Dot, namePredicate, "tchbas_p_Dif_sw.svg")
  }

  it should "create a graph centred on BMWtchctr_pct_Wg" in {
    val namePredicate: String => Boolean = s => s.startsWith("BMWtchctr_pct_Wg")
    valueCentredGraphWith(a2l2Dot, namePredicate, "BMWtchctr_pct_Wg.svg")
  }

  it should "create a valvelift graph on GKF_EHUB_NORM_WARM_LAST" in {
    val fnPredicate: String => Boolean =
      s => s == "BMW_MOD_BlsHub_HubevsollKF_10ms"
    val namePredicate: String => Boolean = s => s == "GKF_EHUB_NORM_WARM_LAST"
    functionCentredGraphWith(a2l2Dot, _ => true, fnPredicate, "BlsHub_HubevsollKF_10ms.svg")
  }

  it should "create a graph on P_MDGI_HAXL and BMW_MOD_Mafw_MdMax" in {
    val fnPredicate: String => Boolean =
      s => s == "BMW_MOD_Mafw_MdMax" || s == "P_MDGI_HAXL"

    functionCentredGraphWith(a2l2Dot, _ => true, fnPredicate, "P_MDGI_HAXL-Mafw_MdMax.svg")
  }

  it should "graph torque request ceiling" in {
    val namePredicate: String => Boolean = _ == "BMWtqe_tqc_FlApplStgNorm_T"
    valueCentredGraphWith(a2l2Dot, namePredicate, "BMWtqe_tqc_FlApplStgNorm_T.svg")
  }

  it should "graph BMW_MOD_TqeLimStatMaxMdk_10ms" in {
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "BMW_MOD_TqeLimStatMaxMdk_10ms", "TqeLimStatMaxMdk_10ms.svg")
  }

  it should "create a graph on Nkw_opt" in {
    val namePredicate: String => Boolean = s => s == "Nkw_opt"
    valueCentredGraphWith(a2l2Dot, namePredicate, "Nkw_opt.svg")
  }

  it should "create a graph for K_FRFMXBS_MN" in {
    valueCentredGraphWith(a2l2Dot, _ == "K_FRFMXBS_MN", "K_FRFMXBS_MN.svg")
  }

  it should "create a graph for BMW_MOD_BsPost_20" in {
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "BMW_MOD_BsPost_200ms", "BsPost_200ms.svg")
  }

  it should "create a graph for KL_MDRED_NKW" in {
    valueCentredGraphWith(a2l2Dot, _ == "KL_MDRED_NKW", "KL_MDRED_NKW.svg")
  }

  it should "create a graph for wmschutzvma" in {
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "wmschutzvma", "wmschutzvma.svg")
  }

  it should "create a graph for K_MDKIST_SOT_MX" in {
    valueCentredGraphWith(a2l2Dot, _ == "K_MDKIST_SOT_MX", "K_MDKIST_SOT_MX.svg")
  }

  it should "create a graph for BMW_MOD_AsInFahrsit_100ms" in {
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "BMW_MOD_AsInFahrsit_100ms", "AsInFahrsit_100ms.svg")
  }

  it should "graph Pwg_ist" in {
    valueCentredGraphWith(a2l2Dot, _ == "Pwg_ist", "Pwg_ist.svg")

    val namePred: String => Boolean = s => s == "Mdk_ist" || s == "Pwg_ist"
    val fnPred: String => Boolean =
      s => s == "BMW_MOD_AsInFahrsit_100ms" || s == "P_MDKIST_10ms" || s == "BMW_SWC_MDIST_Int" || s == "layer"
    functionCentredGraphWith(a2l2Dot, s => true, fnPred, "mdk_pwg_ist.svg")
  }

  it should "graph MoFTrqPtd_tqCluMax_C" in {
    valueCentredGraphWith(a2l2Dot, _ == "MoFTrqPtd_tqCluMax_C", "MoFTrqPtd_tqCluMax_C.svg")
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "MoFDrDem_Co", "MoFDrDem_Co.svg")
  }

  it should "graph KF_MDIOP_1_TQE" in {
    valueCentredGraphWith(a2l2Dot, _ == "KF_MDIOP_1_TQE", "KF_MDIOP_1_TQE.svg")
    valueCentredGraphWith(a2l2Dot, _ == "Md_reib_vm", "Md_reib_vm.svg")
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "BMW_MOD_TqeBasTqiMax_seg", "TqeBasTqiMax_seg.svg")
    functionCentredGraphWith(
      a2l2Dot,
      _ => true,
      s => s == "BMW_SWC_TqeLimStat_Int" || s == "BMW_MOD_TqeBasTqiMax_seg",
      "TqeLimStat_Int.svg"
    )
  }

  it should "graph K_EDA_P_ANZ_SPORT_BS_SCAL_CODE" in {
    valueCentredGraphWith(a2l2Dot, _.contains("K_EDA_P_ANZ_SPORT_BS_SCAL_CODE"), "K_EDA_P_ANZ_SPORT_BS_SCAL_CODE.svg")
    functionCentredGraphWith(a2l2Dot, _ => true, _ == "BMW_MOD_Abk_Tqc_Lda", "Abk_Tqc_Lda.svg")
  }
}

object A2l2DotTest {

  def valueCentredGraphWith(
      a2l2Dot: A2l2Dot,
      namePredicate: String => Boolean,
      filename: String
  ): Unit = {
    val graph = a2l2Dot.valueCentredGraph(namePredicate)

    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File(filename))
  }

  def functionCentredGraphWith(
      a2l2Dot: A2l2Dot,
      namePredicate: String => Boolean,
      fnPredicate: String => Boolean,
      filename: String
  ): Unit = {
    val graph = a2l2Dot.functionCentredGraph(fnPredicate, namePredicate)

    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File(filename))
  }
}