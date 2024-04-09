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
