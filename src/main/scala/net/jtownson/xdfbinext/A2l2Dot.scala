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
import org.apache.commons.text.WordUtils

import java.io.File
import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.util.Using

class A2l2Dot(a2lUrl: URL) {

  val a2l: Asap2File = getA2L()

  val compuMethods: Map[String, CompuMethod] = a2l
    .iterator()
    .asScala
    .collect { case c: CompuMethod => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val characteristics: Map[String, Characteristic] = a2l
    .iterator()
    .asScala
    .collect { case c: Characteristic => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val measurements: Map[String, Measurement] = a2l
    .iterator()
    .asScala
    .collect { case c: Measurement => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val axisPts: Map[String, AxisPts] = a2l
    .iterator()
    .asScala
    .collect { case c: AxisPts => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val functions: Map[String, Function] = a2l
    .iterator()
    .asScala
    .collect { case f: Function => f }
    .map(f => f.getName -> f)
    .toMap

  def characteristic2XDF(namePredicate: String => Boolean): Seq[String] = {
    val applicableCharacteristics = characteristics.filter((n, _) => namePredicate(n)).values

    applicableCharacteristics.map(characteristic2XDF).toSeq
  }

  private def characteristicFold[T](
      c: Characteristic,
      fValue: Characteristic => T,
      fCurve: Characteristic => T,
      fMap: Characteristic => T
  ): T = {
    if (c.getType == CharacteristicType.VALUE)
      fValue(c)
    else if (c.getType == CharacteristicType.CURVE)
      fCurve(c)
    else if (c.getType == CharacteristicType.MAP)
      fMap(c)
    else
      throw new IllegalStateException(s"Unsupported characteristic type: ${c.getType}")
  }

  def characteristic2XDF(c: Characteristic): String = {
    characteristicFold(c, value2Xdf, curve2Xdf, map2Xdf)
  }

  def offsetAddress(a2lAddress: Long): String = {
    s"0x${(a2lAddress - 0x9000000).toHexString}"
  }

  def value2Xdf(c: Characteristic): String = {
    val title       = c.getName
    val description = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod = compuMethods(c.getConversion)
    val units       = compuMethod.getUnit
    val decimalPl   = format2DecimalPl(compuMethod.getFormat)
    val outputType  = "1"
    val min         = BigDecimal(c.getLowerLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val max         = BigDecimal(c.getUpperLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val eq          = "X"
    val typeFlag    = "0x01"
    val address     = offsetAddress(c.getAddress)
    val elSizeBits  = "16"
    A2l2Dot.xdfValueTemplate(
      title,
      description,
      units,
      decimalPl,
      outputType,
      min,
      max,
      eq,
      typeFlag,
      address,
      elSizeBits
    )
  }

  private val formatExpr = """%(\d+)\.(\d+)""".r

  private def format2DecimalPl(format: String): String = {
    format match
      case formatExpr(pre, suf) =>
        suf
  }

  def curve2Xdf(c: Characteristic): String = {
    val title       = c.getName
    val description = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod = compuMethods(c.getConversion)
    val units       = compuMethod.getUnit
    val decimalPl   = format2DecimalPl(compuMethod.getFormat)
    val outputType  = "1"
    val min         = BigDecimal(c.getLowerLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val max         = BigDecimal(c.getUpperLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val eq          = "X"
    val typeFlag    = "0x01"
    val address     = offsetAddress(c.getAddress)
    val elSizeBits  = "16"

    val xAxisDescr  = c.getAxisDescriptions.asScala(0)
    val xAxisRef    = xAxisDescr.getAxisPoints_ref
    val xAxis       = axisPts(xAxisRef)
    val maybeXCompu = compuMethods.get(xAxis.getConversion)
    val xUnits      = maybeXCompu.map(_.getUnit).getOrElse("-")
    val xTitle      = xAxis.getName
    val xDesc       = xAxis.getLongIdentifier
    val xDp         = format2DecimalPl(xAxis.getFormat)
    val xMin        = BigDecimal(xAxisDescr.getLowerLimit).setScale(xDp.toInt, HALF_UP).toString
    val xMax        = BigDecimal(xAxisDescr.getUpperLimit).setScale(xDp.toInt, HALF_UP).toString
    val xAddr       = offsetAddress(xAxis.getAddress)
    val xElSzBits   = "16"
    val xTypeFlag   = "0x01"
    val xColCount   = xAxis.getMaxAxisPoints.toString

    val sb: StringBuilder = new StringBuilder()

    sb.append(
      A2l2Dot.xdfCurveTemplate(
        title,
        description,
        units,
        decimalPl,
        outputType,
        min,
        max,
        eq,
        typeFlag,
        address,
        elSizeBits,
        xAddr,
        xElSzBits,
        xTypeFlag,
        xColCount,
        xUnits,
        "X"
      )
    )

    sb.append(
      A2l2Dot.xdfAxisTemplate(xTitle, xDesc, xAddr, xColCount, xUnits, xDp, xMin, xMax)
    )

    sb.toString
  }

  def map2Xdf(c: Characteristic): String = {
    val title       = c.getName
    val description = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod = compuMethods(c.getConversion)
    val units       = compuMethod.getUnit
    val decimalPl   = format2DecimalPl(compuMethod.getFormat)
    val outputType  = "1"
    val min         = BigDecimal(c.getLowerLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val max         = BigDecimal(c.getUpperLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val eq          = "X"
    val typeFlag    = "0x01"
    val address     = offsetAddress(c.getAddress)
    val elSizeBits  = "16"

    val xAxisDescr  = c.getAxisDescriptions.asScala(0)
    val xAxisRef    = xAxisDescr.getAxisPoints_ref
    val xAxis       = axisPts(xAxisRef)
    val maybeXCompu = compuMethods.get(xAxis.getConversion)
    val xUnits      = maybeXCompu.map(_.getUnit).getOrElse("-")
    val xTitle      = xAxis.getName
    val xDesc       = xAxis.getLongIdentifier
    val xDp         = format2DecimalPl(xAxis.getFormat)
    val xMin        = BigDecimal(xAxisDescr.getLowerLimit).setScale(xDp.toInt, HALF_UP).toString
    val xMax        = BigDecimal(xAxisDescr.getUpperLimit).setScale(xDp.toInt, HALF_UP).toString
    val xAddr       = offsetAddress(xAxis.getAddress)
    val xElSzBits   = "16"
    val xTypeFlag   = "0x01"
    val xColCount   = xAxis.getMaxAxisPoints.toString

    val yAxisDescr  = c.getAxisDescriptions.asScala(1)
    val yAxisRef    = yAxisDescr.getAxisPoints_ref
    val yAxis       = axisPts(yAxisRef)
    val maybeYCompu = compuMethods.get(yAxis.getConversion)
    val yUnits      = maybeYCompu.map(_.getUnit).getOrElse("-")
    val yTitle      = yAxis.getName
    val yDesc       = yAxis.getLongIdentifier
    val yDp         = format2DecimalPl(yAxis.getFormat)
    val yMin        = BigDecimal(yAxisDescr.getLowerLimit).setScale(xDp.toInt, HALF_UP).toString
    val yMax        = BigDecimal(yAxisDescr.getUpperLimit).setScale(xDp.toInt, HALF_UP).toString
    val yAddr       = offsetAddress(yAxis.getAddress)
    val yElSzBits   = "16"
    val yTypeFlag   = "0x01"
    val yColCount   = yAxis.getMaxAxisPoints.toString

    val sb = new StringBuilder

    sb.append(
      A2l2Dot.xdfMapTemplate(
        title,
        description,
        units,
        decimalPl,
        outputType,
        min,
        max,
        eq,
        typeFlag,
        address,
        elSizeBits,
        xAddr,
        xElSzBits,
        xTypeFlag,
        xColCount,
        xUnits,
        "X",
        yAddr,
        yElSzBits,
        yTypeFlag,
        yColCount,
        yUnits,
        "X"
      )
    )

    sb.append(A2l2Dot.xdfAxisTemplate(xTitle, xDesc, xAddr, xColCount, xUnits, xDp, xMin, xMax))

    sb.append(A2l2Dot.xdfAxisTemplate(yTitle, yDesc, yAddr, yColCount, yUnits, yDp, yMin, yMax))

    sb.toString
  }

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

  private def getObjectDescription(name: String, default: String): String = {
    BmwTchDescriptions.table.getOrElse(name, default)
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

  def xdfValueTemplate(
      title: String,
      description: String,
      units: String,
      decimalPl: String,
      outputType: String,
      min: String,
      max: String,
      equation: String,
      typeFlag: String,
      address: String,
      elSzBits: String
  ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
      |    <title>$title</title>
      |    <description>$description</description>
      |    <XDFAXIS id="x" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedelementsizebits="8" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>-</units>
      |      <indexcount>1</indexcount>
      |      <decimalpl>-1</decimalpl>
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <LABEL index="0" value="0.000000" />
      |      <MATH equation="X">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="y" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedelementsizebits="8" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>-</units>
      |      <indexcount>1</indexcount>
      |      <decimalpl>-1</decimalpl>
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <LABEL index="0" value="0.000000" />
      |      <MATH equation="X">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="z">
      |      <EMBEDDEDDATA mmedtypeflags="$typeFlag" mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$units</units>
      |      <decimalpl>$decimalPl</decimalpl>
      |      <min>$min</min>
      |      <max>$max</max>
      |      <outputtype>$outputType</outputtype>
      |      <MATH equation="$equation">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |  </XDFTABLE>
      |""".stripMargin
  }

  def xdfCurveTemplate(
      title: String,
      description: String,
      units: String,
      decimalPl: String,
      outputType: String,
      min: String,
      max: String,
      equation: String,
      typeFlag: String,
      address: String,
      elSzBits: String,
      xAddr: String,
      xElSzBits: String,
      xTypeFlag: String,
      xColCount: String,
      xUnits: String,
      xEquation: String
  ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
      |    <title>$title</title>
      |    <description>$description</description>
      |    <XDFAXIS id="x" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedtypeflags="$xTypeFlag" mmedaddress="$xAddr" mmedelementsizebits="$xElSzBits" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$xUnits</units>
      |      <indexcount>$xColCount</indexcount>
      |      <decimalpl>0</decimalpl>
      |      <embedinfo type="1" />
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <MATH equation="$xEquation">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="y" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedelementsizebits="8" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>-</units>
      |      <indexcount>1</indexcount>
      |      <decimalpl>-1</decimalpl>
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <LABEL index="0" value="0.000000" />
      |      <MATH equation="X*1">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="z">
      |      <EMBEDDEDDATA mmedtypeflags="$typeFlag" mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="1" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$units</units>
      |      <decimalpl>$decimalPl</decimalpl>
      |      <min>$min</min>
      |      <max>$max</max>
      |      <outputtype>1</outputtype>
      |      <MATH equation="$equation">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |  </XDFTABLE>
      |""".stripMargin
  }

  def xdfMapTemplate(
      title: String,
      description: String,
      units: String,
      decimalPl: String,
      outputType: String,
      min: String,
      max: String,
      equation: String,
      typeFlag: String,
      address: String,
      elSzBits: String,
      xAddr: String,
      xElSzBits: String,
      xTypeFlag: String,
      xColCount: String,
      xUnits: String,
      xEquation: String,
      yAddr: String,
      yElSzBits: String,
      yTypeFlag: String,
      yColCount: String,
      yUnits: String,
      yEquation: String
  ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
      |    <title>$title</title>
      |    <description>$description</description>
      |    <XDFAXIS id="x" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedtypeflags="$xTypeFlag" mmedaddress="$xAddr" mmedelementsizebits="$xElSzBits" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$xUnits</units>
      |      <indexcount>$xColCount</indexcount>
      |      <decimalpl>0</decimalpl>
      |      <embedinfo type="1" />
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <MATH equation="X">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="y" uniqueid="0x0">
      |      <EMBEDDEDDATA mmedaddress="$yAddr" mmedelementsizebits="$yElSzBits" mmedcolcount="$yColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$yUnits</units>
      |      <indexcount>$yColCount</indexcount>
      |      <decimalpl>1</decimalpl>
      |      <embedinfo type="1" />
      |      <datatype>0</datatype>
      |      <unittype>0</unittype>
      |      <DALINK index="0" />
      |      <MATH equation="X">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |    <XDFAXIS id="z">
      |      <EMBEDDEDDATA mmedtypeflags="$typeFlag" mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="1" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
      |      <units>$units</units>
      |      <decimalpl>$decimalPl</decimalpl>
      |      <min>$min</min>
      |      <max>$max</max>
      |      <outputtype>1</outputtype>
      |      <MATH equation="$equation">
      |        <VAR id="X" />
      |      </MATH>
      |    </XDFAXIS>
      |  </XDFTABLE>
      |""".stripMargin
  }

  def label(i: Int): String = s"""<LABEL index="$i" value="0.00" />"""

  val newline = "\n"

  def xdfAxisTemplate(
      title: String,
      description: String,
      address: String,
      count: String,
      units: String,
      decimalPl: String,
      min: String,
      max: String
  ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
       |    <title>$title</title>
       |    <description>$description</description>
       |    <XDFAXIS id="x" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="16" mmedmajorstridebits="-32" mmedminorstridebits="0" />
       |      <indexcount>$count</indexcount>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      ${(0 until count.toInt).map(label).mkString(newline)}
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="y" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="16" mmedmajorstridebits="-32" mmedminorstridebits="0" />
       |      <indexcount>1</indexcount>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <LABEL index="0" value="0.00" />
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="z">
       |      <EMBEDDEDDATA mmedtypeflags="0x01" mmedaddress="$address" mmedelementsizebits="16" mmedrowcount="1" mmedcolcount="14" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$units</units>
       |      <decimalpl>$decimalPl</decimalpl>
       |      <min>$min</min>
       |      <max>$max</max>
       |      <outputtype>1</outputtype>
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |  </XDFTABLE>
       |""".stripMargin
  }
}
