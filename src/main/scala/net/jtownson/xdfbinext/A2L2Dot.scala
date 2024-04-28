package net.jtownson.xdfbinext

import guru.nidi.graphviz.attribute.*
import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.Rank.RankDir.LEFT_TO_RIGHT
import guru.nidi.graphviz.attribute.Shape.RECTANGLE
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory.{mutGraph, mutNode}
import guru.nidi.graphviz.model.MutableGraph
import net.alenzen.a2l.enums.CharacteristicType
import net.alenzen.a2l.{Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2LWrapper.getObjectDescription
import org.apache.commons.text.WordUtils

import java.io.File
import java.net.URL
import scala.jdk.CollectionConverters.*

class A2L2Dot(a2lUrl: URL) {

  private val a2l             = A2LWrapper(a2lUrl)
  private val compuMethods    = a2l.compuMethods
  private val characteristics = a2l.characteristics
  private val measurements    = a2l.measurements
  private val axisPts         = a2l.axisPts
  private val functions       = a2l.functions

  def valueCentredGraph(namePredicate: String => Boolean): MutableGraph = {

    def nn(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    val applicableFns = functions.filter { (name, fn) =>
      val defCharacteristics = nn(fn.getDefCharacteristics)
      val inMeasurements     = nn(fn.getInMeasurments)
      val locMeasurements    = nn(fn.getLocMeasurments)
      val outMeasurements    = nn(fn.getOutMeasurments)

      defCharacteristics.exists(namePredicate) ||
      inMeasurements.exists(namePredicate) ||
      locMeasurements.exists(namePredicate) ||
      outMeasurements.exists(namePredicate)
    }.keySet

    functionCentredGraph(fnName => applicableFns.contains(fnName), _ => true)
  }

  def functionCentredGraph(fnPredicate: String => Boolean, namePredicate: String => Boolean): MutableGraph = {

    def nfm(l: IdentReferenceList): Seq[String] = {
      Option(l).fold(Seq.empty[String])(_.iterator().asScala.toSeq.filter(namePredicate))
    }

    val graph: MutableGraph =
      mutGraph(a2l.a2l.getProject.getName).setDirected(true).graphAttrs().add(Rank.dir(LEFT_TO_RIGHT))

    a2l.a2l.iterator().asScala.foreach {
      case n: net.alenzen.a2l.Function =>
        if (fnPredicate(n.getName)) {
          val gn = mutNode(n.getName).add(Style.lineWidth(4))

          val defCharacteristics = nfm(n.getDefCharacteristics)
          val inMeasurements     = nfm(n.getInMeasurments)
          val locMeasurements    = nfm(n.getLocMeasurments)
          val outMeasurements    = nfm(n.getOutMeasurments)

          defCharacteristics.foreach { cn =>
            characteristics.get(cn).foreach { c =>
              val node = characteristicNode(c, graph).addLink(gn)
              graph.add(node)
            }
          }
          inMeasurements.foreach { m =>
            val a2lM = measurements(m)
            val node = measurementNode(a2lM).addLink(gn)
            graph.add(node)
          }
          locMeasurements.foreach { m =>
            val a2lM = measurements(m)
            val node = measurementNode(a2lM)
            gn.addLink(node)
            graph.add(node)
          }
          outMeasurements.foreach { m =>
            val a2lM = measurements(m)
            val node = measurementNode(a2lM)
            gn.addLink(node)
            graph.add(node)
          }

          graph.add(gn)
        }
      case n =>
    }
    graph
  }

  private def mapLabel(name: String, units: String, longDescription: String): Label = {
    Label.lines(s"$name", s"Units: $units", WordUtils.wrap(longDescription, 80))
  }

  private def characteristicNode(c: Characteristic, graph: MutableGraph) = {
    if (!BmwTchDescriptions.table.contains(c.getName)) {
      println(s"""${c.getName},"${c.getLongIdentifier}"""")
    }

    val gn    = mutNode(c.getName)
    val units = compuMethods.get(c.getConversion).map(_.getUnit).getOrElse("-")
    if (c.getType == CharacteristicType.MAP) {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Color.BLUE
      )
    } else if (c.getType == CharacteristicType.CURVE) {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Color.BLUE
      )
    } else {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Color.BLUE
      )
    }

    def addAxisIndex(i: Int): Unit = {
      val axisRef = c.getAxisDescriptions.asScala(i).getAxisPoints_ref
      axisPts.get(axisRef).foreach { axis =>
        val n = axisPtsNode(axis).addLink(gn)
        graph.add(n)
      }
    }

    if (c.getType == CharacteristicType.CURVE) {
      addAxisIndex(0)
    } else if (c.getType == CharacteristicType.MAP) {
      addAxisIndex(0)
      addAxisIndex(1)
    }
    gn
  }

  private def measurementNode(m: Measurement) = {
    val gn    = mutNode(m.getName)
    val units = compuMethods.get(m.getConversion).map(_.getUnit).getOrElse("-")

    if (!BmwTchDescriptions.table.contains(m.getName)) {
      println(s"""${m.getName},"${m.getLongIdentifier}"""")
    }
    val longDescription = BmwTchDescriptions.table.getOrElse(m.getName, "")
    gn.add(
      mapLabel(name = m.getName, units = units, longDescription = longDescription),
      Color.RED
    )
    gn
  }

  private def axisPtsNode(n: AxisPts) = {
    val gn    = mutNode(n.getName)
    val units = compuMethods.get(n.getConversion).map(_.getUnit).getOrElse("-")
    gn.add(
      mapLabel(
        name = n.getName,
        units = units,
        longDescription = BmwTchDescriptions.table.getOrElse(n.getName, n.getName)
      ),
      Color.GREY
    )
    gn
  }
}

object A2L2Dot {
  // format: off
  def valueCentredGraphWith(
                             a2l2Dot: A2L2Dot,
                             namePredicate: String => Boolean,
                             filename: String
  ): Unit = {
    val graph = a2l2Dot.valueCentredGraph(namePredicate)
    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File(filename))
  }

  def functionCentredGraphWith(
                                a2l2Dot: A2L2Dot,
                                namePredicate: String => Boolean,
                                fnPredicate: String => Boolean,
                                filename: String
  ): Unit = {
    val graph = a2l2Dot.functionCentredGraph(fnPredicate, namePredicate)
    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File(filename))
  }

}
