package net.jtownson.xdfbinext

import guru.nidi.graphviz.attribute.Image.{Position, Scale}
import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.Rank.RankDir.LEFT_TO_RIGHT
import guru.nidi.graphviz.attribute.Shape.RECTANGLE
import guru.nidi.graphviz.attribute.{Color, Font, Image, Label, Rank, Style}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory.{mutGraph, mutNode}
import guru.nidi.graphviz.model.MutableGraph
import net.alenzen.a2l.enums.CharacteristicType
import net.alenzen.a2l.{Unit as A2lUnit, *}
import org.apache.commons.text.WordUtils

import java.io.File
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

  private val functions: Map[String, Function] = a2l
    .iterator()
    .asScala
    .collect { case f: Function => f }
    .map(f => f.getName -> f)
    .toMap

  //    axisPts.foreach { (n, m) =>
  //      if (namePredicate(n))
  //        println(s"""$n,"${m.getLongIdentifier}"""")
  //    }
  //    fail()

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
      mutGraph(a2l.getProject.getName).setDirected(true).graphAttrs().add(Rank.dir(LEFT_TO_RIGHT))

    a2l.iterator().asScala.foreach {
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

  private def getA2L(): Asap2File = {
    Using.resource(a2lUrl.openStream()) { i =>
      val parser: Asap2Parser = new Asap2Parser(i)
      parser.parse()
    }
  }

  private def mapLabel(name: String, units: String, longDescription: String): Label = {
    Label.lines(s"$name", s"Units: $units", WordUtils.wrap(longDescription, 80))
  }

  private def getObjectDescription(name: String): String = {
    BmwTchDescriptions.table.get(name) match
      case Some(value) =>
        value
      case None =>
//        println(s"Missing $name")
        ""
  }

  private def characteristicNode(c: Characteristic, graph: MutableGraph) = {
    if (!BmwTchDescriptions.table.contains(c.getName)) {
      println(s"""${c.getName},"${c.getLongIdentifier}"""")
    }

    val gn    = mutNode(c.getName)
    val units = compuMethods.get(c.getConversion).map(_.getUnit).getOrElse("-")
    if (c.getType == CharacteristicType.MAP) {
      gn.add(
        mapLabel(name = c.getName, units = units, longDescription = getObjectDescription(c.getName)),
        RECTANGLE,
//        Image.of("./axis-xyz.png").position(Position.TOP_RIGHT).scale(Scale.NONE),
        Color.BLUE
      )
    } else if (c.getType == CharacteristicType.CURVE) {
      gn.add(
        mapLabel(name = c.getName, units = units, longDescription = getObjectDescription(c.getName)),
        RECTANGLE,
//        Image.of("./axis-xy.png").position(Position.TOP_RIGHT).scale(Scale.NONE),
        Color.BLUE
      )
    } else {
      gn.add(
        mapLabel(name = c.getName, units = units, longDescription = getObjectDescription(c.getName)),
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

  def asCommentCsv(namePredicate: String => Boolean) = {
    a2l
      .iterator()
      .asScala
      .foreach(n => fold(n, namePredicate))
  }

  private def print(name: String, ld: String, namePredicate: String => Boolean) = {
    if (namePredicate(name) && (!BmwTchDescriptions.table.contains(name)) && ld.trim.nonEmpty) {
      println(s""""$name","$ld"""")
    }
  }

  private def fold(node: IAsap2TreeElement, namePredicate: String => Boolean) = node match {
    case n: net.alenzen.a2l.CompuMethod =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.CompuTab =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Project =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.AxisPts =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.MemorySegment =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Measurement =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Function =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Characteristic =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Module =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.CompuVTab =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n: net.alenzen.a2l.Unit =>
      val name = n.getName
      val ld   = n.getLongIdentifier
      print(name, ld, namePredicate)
    case n =>
  }
}

object A2l2Dot {

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
