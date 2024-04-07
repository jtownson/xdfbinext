package net.jtownson.xdfbinext

import guru.nidi.graphviz.engine.Format
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.jdk.CollectionConverters.*
import guru.nidi.graphviz.engine.Graphviz

class A2LParserTest extends AnyFlatSpec {

  behavior of "A2LParser"

  it should "work" in {

    val a2lUrl = getClass.getResource("/DME861_R1C9J9E3B.a2l").toURI.toURL

    val namePredicate: String => Boolean =
      s => s.startsWith("BMWtchctr_pct") || s.startsWith("BMWtchctr_pwr") // || s.startsWith("BMWewg")

    val fnPredicate: String => Boolean =
      s => s == "BMW_MOD_TchCtr_Pwr2Pos_10ms" || s == "BMW_MOD_TchCtr_Pos_10ms" || s == "BMW_MOD_TchCtr_Pwr_10ms"

    val a2l2Dot = new A2l2Dot(a2lUrl)

    val graph = a2l2Dot.asGraph(fnPredicate, namePredicate)

    println(graph.toString)
    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File("TchCtr-3.svg"))
  }
}
