package net.jtownson.xdfbinext

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.MutableGraph
import guru.nidi.graphviz.parse.Parser
import net.jtownson.xdfbinext.A2l2Dot.{functionCentredGraphWith, valueCentredGraphWith}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks.*

import java.io.File
import scala.util.Using

class A2l2DotTest extends AnyFlatSpec {

  behavior of "A2l2Dot"

  private val a2lUrl  = getClass.getResource("/DME861_R1C9J9E3B.a2l").toURI.toURL
  private val a2l2Dot = new A2l2Dot(a2lUrl)

  private val handGraphs = Table[String](
    "filename",
    "tch-pwr-ff.dot",
    "tch-pwr-p.dot",
    "tch-pwr-d.dot",
    "tch-pwr.dot",
    "wg-basc.dot"
  )

  private val fnCentredGraphs = Table[String](
    "functionName",
    "BMW_MOD_TchSp_P_10ms",
    "BMW_MOD_TchBas_P_10ms",
    "BMW_MOD_TchBas_Misc_10ms",
    "BMW_MOD_BlsRfMax_10ms",
    "BMW_SWC_Ewg",
    "BMW_MOD_TchCtr_Pwr_10ms",
    "BMW_MOD_TchCtr_PosAdIp_10ms",
    "BMW_MOD_TchOut_10ms",
    "BMW_MOD_TqeLimStatMaxMdk_10ms",
    "BMW_MOD_BsPost_200ms"
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

  it should "render hand drawn graphs" ignore forAll(handGraphs) { filename =>
    A2l2DotTest.resourcesGraph(filename)
  }

  it should "render fn centred graphs" ignore forAll(fnCentredGraphs) { fnName =>
    functionCentredGraphWith(a2l2Dot, _ => true, _ == fnName, s"${fnName}.svg")
  }

  it should "render value centred graphs" ignore forAll(valueCentredGraphs) { objName =>
    valueCentredGraphWith(a2l2Dot, _ == objName, s"${objName}.svg")
  }
}

object A2l2DotTest {

  def resourcesGraph(filename: String): Unit = {
    Using.resource(classOf[A2l2DotTest].getResourceAsStream(s"/$filename")) { dot =>
      val g: MutableGraph = new Parser().read(dot)
      Graphviz.fromGraph(g).render(Format.SVG).toFile(new File(s"$filename.svg"))
    }
  }
}
