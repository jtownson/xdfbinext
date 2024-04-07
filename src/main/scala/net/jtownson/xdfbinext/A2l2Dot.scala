package net.jtownson.xdfbinext

import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.Rank.RankDir.LEFT_TO_RIGHT
import guru.nidi.graphviz.attribute.{Label, Rank}
import guru.nidi.graphviz.model.Factory.{mutGraph, mutNode}
import guru.nidi.graphviz.model.MutableGraph
import net.alenzen.a2l.enums.CharacteristicType
import net.alenzen.a2l.*
import org.apache.commons.text.WordUtils

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.util.Using

class A2l2Dot(a2lUrl: URL) {

  private val a2l: Asap2File = getA2L()

  private val compuMethods: Map[String, CompuMethod] = a2l
    .iterator()
    .asScala
    .collect { case c: CompuMethod => c }
    .map(cm => cm.getName -> cm)
    .toMap

  private val characteristics: Map[String, Characteristic] = a2l
    .iterator()
    .asScala
    .collect { case c: Characteristic => c }
    .map(cm => cm.getName -> cm)
    .toMap

  private val measurements: Map[String, Measurement] = a2l
    .iterator()
    .asScala
    .collect { case c: Measurement => c }
    .map(cm => cm.getName -> cm)
    .toMap

  private val axisPts: Map[String, AxisPts] = a2l
    .iterator()
    .asScala
    .collect { case c: AxisPts => c }
    .map(cm => cm.getName -> cm)
    .toMap

  //    axisPts.foreach { (n, m) =>
  //      if (namePredicate(n))
  //        println(s"""$n,"${m.getLongIdentifier}"""")
  //    }
  //    fail()

  def asGraph(fnPredicate: String => Boolean, namePredicate: String => Boolean): MutableGraph = {

    val graph: MutableGraph =
      mutGraph(a2l.getProject.getName).setDirected(true).graphAttrs().add(Rank.dir(LEFT_TO_RIGHT))

    a2l.iterator().asScala.foreach {
      case n: net.alenzen.a2l.Function if (fnPredicate(n.getName)) =>
        // create a node for the function
        // draw lines from each defChar to the function
        // draw lines from each inMeasurem to the function
        // draw lines from the function to each outMesurement
        val gn = mutNode(n.getName)

        val defCharacteristics = n.getDefCharacteristics.iterator().asScala.filter(namePredicate)
        val inMeasurements     = n.getInMeasurments.iterator().asScala.filter(namePredicate)
        val locMeasurements    = n.getLocMeasurments.iterator().asScala.toSeq.filter(namePredicate)
        val outMeasurements    = n.getOutMeasurments.iterator().asScala.toSeq.filter(namePredicate)

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

      case n =>

    }
    graph
  }

  private def getA2L(): Asap2File = {
    Using.resource(a2lUrl.openStream()) { i =>
      val parser: Asap2Parser = new Asap2Parser(i)
      parser.parse()
    }
  }

  private def mapLabel(name: String, units: String, longDescription: String): Label = {
    Label.lines(s"$name", s"Units: $units", WordUtils.wrap(longDescription, 80))
  }

  private def characteristicNode(c: Characteristic, graph: MutableGraph) = {
    val gn    = mutNode(c.getName)
    val units = compuMethods.get(c.getConversion).map(_.getUnit).getOrElse("-")
    gn.add(mapLabel(name = c.getName, units = units, longDescription = BmwTchDescriptions.table(c.getName)))

    def addAxisIndex(i: Int) = {
      val axisRef = c.getAxisDescriptions.asScala(i).getAxisPoints_ref
      val axis    = axisPts(axisRef)
      val n       = axisPtsNode(axis).addLink(gn)
      graph.add(n)
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
    gn.add(mapLabel(name = m.getName, units = units, longDescription = BmwTchDescriptions.table(m.getName)))
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
      )
    )
    gn
  }
}
