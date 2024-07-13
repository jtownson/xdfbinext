package net.jtownson.xdfbinext

import guru.nidi.graphviz.attribute.*
import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.Rank.RankDir.LEFT_TO_RIGHT
import guru.nidi.graphviz.attribute.Shape.RECTANGLE
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory.{mutGraph, mutNode}
import guru.nidi.graphviz.model.{MutableGraph, MutableNode}
import net.alenzen.a2l.enums.CharacteristicType
import net.alenzen.a2l.{Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2L2Dot.MeasurementRole.{In, InOut, Loc, Out}
import net.jtownson.xdfbinext.A2L2Dot.{GraphOptions, MeasurementPredicate, MeasurementRole, liftNamePredicate}
import net.jtownson.xdfbinext.A2LWrapper.getObjectDescription
import org.apache.commons.text.WordUtils

import java.io.File
import java.net.URL
import java.util.UUID
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class A2L2Dot(a2lUrl: URL) {

  val a2l: A2LWrapper         = A2LWrapper(a2lUrl)
  private val compuMethods    = a2l.compuMethods
  private val characteristics = a2l.characteristics
  private val measurements    = a2l.measurements
  private val axisPts         = a2l.axisPts
  private val functions       = a2l.functions

  def valueCentredGraph(graphName: String, namePredicate: String => Boolean): MutableGraph = {

    def nn(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    val applicableFns = functions.filter { (name, fn) =>
      val defCharacteristics = nn(fn.getDefCharacteristics)
      val refCharacteristics = nn(fn.getRefCharacteristics)
      val inMeasurements     = nn(fn.getInMeasurments)
      val locMeasurements    = nn(fn.getLocMeasurments)
      val outMeasurements    = nn(fn.getOutMeasurments)

      defCharacteristics.exists(namePredicate) ||
      refCharacteristics.exists(namePredicate) ||
      inMeasurements.exists(namePredicate) ||
      locMeasurements.exists(namePredicate) ||
      outMeasurements.exists(namePredicate)
    }.keySet

    val measurementPred: (String, MeasurementRole) => Boolean = (name, role) => namePredicate(name)

    functionCentredGraph(
      graphName,
      fnName => applicableFns.contains(fnName),
      _ => false,
      liftNamePredicate(namePredicate),
      GraphOptions(false, false)
    )
  }

  def parentFnPredicate(parentFnName: String): String => Boolean = { name =>
    def subFunctionNames(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }
    val parentFn = a2l.functions(parentFnName)

    val subFns = subFunctionNames(parentFn.getSubFunctions)

    subFns.contains(name)
  }

  def parentFnGraph(
      graphName: String,
      fnName: String,
      subFnPred: String => Boolean = Set.empty,
      characteristicPredicate: String => Boolean,
      measurementPredicate: MeasurementPredicate
  ): MutableGraph = {

    def subFunctionNames(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    val parentFn = a2l.functions(fnName)

    val subFns = subFunctionNames(parentFn.getSubFunctions)

    functionCentredGraph(
      graphName,
      n => subFns.contains(n) && subFnPred(n),
      characteristicPredicate,
      measurementPredicate,
      GraphOptions(showAxes = false, showLocals = false)
    )
  }

  def functionCentredGraph(
      graphName: String,
      fnPredicate: String => Boolean,
      characteristicPredicate: String => Boolean,
      measurementPredicate: MeasurementPredicate,
      graphOptions: GraphOptions = GraphOptions(showAxes = true, showLocals = true)
  ): MutableGraph = {

    def nfm(l: IdentReferenceList): Seq[String] = {
      Option(l).fold(Seq.empty[String])(_.iterator().asScala.toSeq)
    }

    val graph: MutableGraph =
      mutGraph(s"${a2l.a2l.getProject.getName}:$graphName").setDirected(true).graphAttrs().add(Rank.dir(LEFT_TO_RIGHT))

    val graphOutNodes: mutable.Set[String]                = mutable.Set.empty
    val graphInNodes: mutable.Set[String]                 = mutable.Set.empty
    val graphInOutNodes: mutable.Map[String, MutableNode] = mutable.Map.empty // inout nodes shared

    // Determine which nodes are outputs so that if they are then
    // inputs to other functions we use the same node.
    functions.foreach { (fn, f) =>
      if (fnPredicate(fn)) {
        val outMeasurements = nfm(f.getOutMeasurments)

        outMeasurements.foreach(graphOutNodes.add)
      }
    }
    functions.foreach { (fn, f) =>
      if (fnPredicate(fn)) {
        val inMeasurements = nfm(f.getInMeasurments)

        inMeasurements.foreach { m =>
          val a2lM = measurements(m)
          if (graphOutNodes.contains(m))
            graphInOutNodes.put(m, measurementNode(a2lM, Color.BLUE))
          else
            graphInNodes.add(m)
        }
      }
    }

    functions.foreach { (fn, n) =>
      if (fnPredicate(n.getName)) {

        val fnNode = mutNode(n.getName).add(Style.lineWidth(4))

        val defCharacteristics = nfm(n.getDefCharacteristics)
        val refCharacteristics = nfm(n.getRefCharacteristics)
        val inMeasurements     = nfm(n.getInMeasurments)
        val locMeasurements    = nfm(n.getLocMeasurments)
        val outMeasurements    = nfm(n.getOutMeasurments)

        defCharacteristics.foreach { cn =>
          characteristics.get(cn).filter(c => characteristicPredicate(c.getName)).foreach { c =>
            val node = characteristicNode(c, graph, graphOptions.showAxes).addLink(fnNode)
            graph.add(node)
          }
        }

        refCharacteristics.foreach { cn =>
          characteristics.get(cn).filter(c => characteristicPredicate(c.getName)).foreach { c =>
            val node = characteristicNode(c, graph, graphOptions.showAxes).addLink(fnNode)
            graph.add(node)
          }
        }

        inMeasurements.foreach { m =>
          val a2lM = measurements(m)
          if (graphInOutNodes.contains(a2lM.getName) && measurementPredicate(a2lM.getName, InOut)) {
            val node = graphInOutNodes(a2lM.getName)
            node.addLink(fnNode)
            graph.add(node)
          } else if (measurementPredicate(a2lM.getName, In)) {
            val node = measurementNode(a2lM, Color.GREEN)
            node.addLink(fnNode)
            graph.add(node)
          }
        }

        if (graphOptions.showLocals) {
          locMeasurements.foreach { m =>
            val a2lM = measurements(m)
            if (
              graphInOutNodes
                .contains(a2lM.getName) && measurementPredicate(a2lM.getName, Loc)
            ) {
              val node = graphInOutNodes(a2lM.getName)
              graphInOutNodes(a2lM.getName)
              fnNode.addLink(node)
              graph.add(node)
            } else if (measurementPredicate(a2lM.getName, Loc)) {
              val node = measurementNode(a2lM, Color.GREY)
              node.addLink(fnNode)
              graph.add(node)
            }
          }
        }

        outMeasurements.foreach { m =>
          val a2lM = measurements(m)

          if (
            graphInOutNodes
              .contains(a2lM.getName) && measurementPredicate(a2lM.getName, InOut)
          ) {
            val node = graphInOutNodes(a2lM.getName)
            fnNode.addLink(node)
            graph.add(node)
          } else if (measurementPredicate(a2lM.getName, Out)) {
            val node = measurementNode(a2lM, Color.RED)
            fnNode.addLink(node)
            graph.add(node)
          }
        }

        graph.add(fnNode)
      }
    }
    graph
  }

  private def mapLabel(name: String, units: String, longDescription: String): Label = {
    Label.lines(s"$name", s"Units: $units", WordUtils.wrap(longDescription, 80))
  }

  private def characteristicNode(c: Characteristic, graph: MutableGraph, showAxes: Boolean = true) = {
    if (!BmwTchDescriptions.table.contains(c.getName)) {
      println(s"""${c.getName},"${c.getLongIdentifier}"""")
    }
    val uid   = UUID.randomUUID()
    val gn    = mutNode(s"${c.getName}:$uid")
    val units = compuMethods.get(c.getConversion).map(_.getUnit).getOrElse("-")
    if (c.getType == CharacteristicType.MAP) {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Style.FILLED,
        Color.ORANGE.fill()
      )
    } else if (c.getType == CharacteristicType.CURVE) {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Style.FILLED,
        Color.ORANGE.fill()
      )
    } else {
      gn.add(
        mapLabel(
          name = c.getName,
          units = units,
          longDescription = getObjectDescription(c.getName, c.getLongIdentifier)
        ),
        RECTANGLE,
        Style.FILLED,
        Color.ORANGE.fill()
      )
    }

    def addAxisIndex(i: Int): Unit = {
      val axisRef = c.getAxisDescriptions.asScala(i).getAxisPoints_ref
      axisPts.get(axisRef).foreach { axis =>
        val n = axisPtsNode(axis).addLink(gn)
        graph.add(n)
      }
    }

    if (showAxes) {
      if (c.getType == CharacteristicType.CURVE) {
        addAxisIndex(0)
      } else if (c.getType == CharacteristicType.MAP) {
        addAxisIndex(0)
        addAxisIndex(1)
      }
    }
    gn
  }

  private def measurementNode(m: Measurement, color: Color) = {
    val uid   = UUID.randomUUID()
    val gn    = mutNode(s"${m.getName}:$uid")
    val units = compuMethods.get(m.getConversion).map(_.getUnit).getOrElse("-")

    if (!BmwTchDescriptions.table.contains(m.getName)) {
      println(s"""${m.getName},"${m.getLongIdentifier}"""")
    }
    val longDescription = BmwTchDescriptions.table.getOrElse(m.getName, "")
    gn.add(
      mapLabel(name = m.getName, units = units, longDescription = longDescription),
      color
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
  def valueCentredGraphWith(graphName: String,
                             a2l2Dot: A2L2Dot,
                             namePredicate: String => Boolean,
                             filename: String
  ): Unit = {
    val graph = a2l2Dot.valueCentredGraph(graphName, namePredicate)
    Graphviz.fromGraph(graph).render(Format.SVG).toFile(new File(filename))
  }

  def functionCentredGraphWith(
                            graphName: String,
                            a2l2Dot: A2L2Dot,
                            fnPredicate: String => Boolean,
                            characteristicPredicate: String => Boolean,
                            measurementPredicate: MeasurementPredicate,
                            graphOptions: GraphOptions = GraphOptions(showAxes = true, showLocals = true),
                            filename: String
                          ): Unit = {
    val graph = a2l2Dot.functionCentredGraph(graphName, fnPredicate, characteristicPredicate, measurementPredicate, graphOptions)
    val gv = Graphviz.fromGraph(graph)
    gv.render(Format.DOT).toFile(new File(s"$filename.dot"))
    gv.render(Format.SVG).toFile(new File(s"$filename.svg"))
    gv.render(Format.PNG).toFile(new File(s"$filename.png"))
//    gv.render(Format.PNG).toFile(new File(s"$filename.png"))
  }

  def parentGraphWith(graphName: String,
                      a2L2Dot: A2L2Dot,
                      fnName: String,
                      filename: String,
                      subFnPred: String => Boolean,
                      characteristicPredicate: String => Boolean,
                      measurementPredicate: MeasurementPredicate): Unit = {
    val graph = a2L2Dot.parentFnGraph(graphName, fnName, subFnPred, characteristicPredicate, measurementPredicate)
    val gv = Graphviz.fromGraph(graph)
    gv.render(Format.DOT).toFile(new File(s"$filename.dot"))
    gv.render(Format.SVG).toFile(new File(s"$filename.svg"))
  }

  case class GraphOptions(showAxes: Boolean, showLocals: Boolean)

  enum MeasurementRole:
    case In, Out, InOut, Loc

  type MeasurementPredicate = (String, MeasurementRole) => Boolean
  
  def liftNamePredicate(namePred: String => Boolean): MeasurementPredicate = (name, _) => namePred(name)
}
