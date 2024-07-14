package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.CharacteristicType.{CURVE, MAP, VALUE, VAL_BLK}
import net.alenzen.a2l.enums.{CharacteristicType, DataType}
import net.alenzen.a2l.{Asap2File, Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2LWrapper.{characteristicFold, getA2L, getObjectDescription}
import net.jtownson.xdfbinext.a2l.CharacteristicSummary
import net.jtownson.xdfbinext.a2l.CharacteristicSummary.{CurveSummary, MapSummary, ValBlkSummary, ValueSummary}

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Using

case class A2LWrapper(a2lUrl: URL) {

  val a2l: Asap2File = getA2L(a2lUrl)

  val recordLayouts: Map[String, RecordLayout] = collectWithPf[RecordLayout](_.getName)

  val compuMethods: Map[String, CompuMethod] = collectWithPf[CompuMethod](_.getName)

  val compuVTabs: Map[String, CompuVTab] = collectWithPf[CompuVTab](_.getName)

  val compuTabs: Map[String, CompuTab] = collectWithPf[CompuTab](_.getName)

  val characteristics: Map[String, Characteristic] = collectWithPf[Characteristic](_.getName)

  val measurements: Map[String, Measurement] = collectWithPf[Measurement](_.getName)

  val axisPts: Map[String, AxisPts] = collectWithPf[AxisPts](_.getName)

  val functions: Map[String, Function] = collectWithPf[Function](_.getName)

  def characteristicUsage(name: String): Set[String] = {
    def nn(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    functions.filter { (fnName, fn) =>
      val fnCharacteristics: Set[String] = nn(fn.getDefCharacteristics) ++ nn(fn.getRefCharacteristics)
      fnCharacteristics.contains(name)
    }.keySet
  }

  def getSummary(name: String): CharacteristicSummary =
    getSummary(characteristics(name))

  private def getSummary(c: Characteristic): CharacteristicSummary = {

    def valueSummary(c: Characteristic) =
      ValueSummary(
        c.getName,
        getObjectDescription(c.getName, c.getLongIdentifier),
        characteristicUsage(c.getName),
        getUnits(c)
      )

    def valBlkSummary(c: Characteristic) =
      ValBlkSummary(
        c.getName,
        getObjectDescription(c.getName, c.getLongIdentifier),
        characteristicUsage(c.getName),
        getUnits(c)
      )

    def curveSummary(c: Characteristic) =
      CurveSummary(
        c.getName,
        getObjectDescription(c.getName, c.getLongIdentifier),
        characteristicUsage(c.getName),
        getXAxisUnits(c),
        getUnits(c)
      )

    def mapSummary(c: Characteristic) =
      MapSummary(
        c.getName,
        getObjectDescription(c.getName, c.getLongIdentifier),
        characteristicUsage(c.getName),
        getXAxisUnits(c),
        getYAxisUnits(c),
        getUnits(c)
      )

    characteristicFold(c, valueSummary, valBlkSummary, curveSummary, mapSummary)

  }

  def getUnits(c: Characteristic): String = {
    val compuMethod = compuMethods(c.getConversion)
    compuMethod.getUnit
  }

  def getXAxisUnits(c: Characteristic): String = {
    getAxisUnits(c.getAxisDescriptions.get(0))
  }

  def getYAxisUnits(c: Characteristic): String = {
    getAxisUnits(c.getAxisDescriptions.get(1))
  }

  def getAxisUnits(axisDescr: AxisDescr): String = {
    compuMethods(axisDescr.getConversion).getUnit
  }

  def getCompuMethod(c: Characteristic): CompuMethod = {
    compuMethods(c.getConversion)
  }

  def getType(c: Characteristic): DataType = {
    val layout: RecordLayout = recordLayouts(c.getDeposit)
    A2LWrapper.getType(layout)
  }

  def getType(a: AxisPts): DataType = {
    val layout: RecordLayout = recordLayouts(a.getDeposit)
    A2LWrapper.getType(layout)
  }

  def getXAxisPts(c: Characteristic): AxisPts = {
    axisPts(c.getAxisDescriptions.get(0).getAxisPoints_ref)
  }

  def getYAxisPts(c: Characteristic): AxisPts = {
    axisPts(c.getAxisDescriptions.get(1).getAxisPoints_ref)
  }

  def getRecordLayout(c: Characteristic): RecordLayout = {
    recordLayouts(c.getDeposit)
  }

  def getRecordLayout(a: AxisPts): RecordLayout = {
    recordLayouts(a.getDeposit)
  }

  def getCellCount(c: Characteristic): Int = {
    if (c.getType == VALUE) {
      1
    } else if (c.getType == CURVE) {
      getXAxisCount(c)
    } else if (c.getType == MAP) {
      getXAxisCount(c) * getYAxisCount(c)
    } else if (c.getType == VAL_BLK) {
      c.getNumber.toInt
    } else {
      ???
    }
  }

  def getXAxisCount(c: Characteristic): Int = {
    getAxisCount(c, 0)
  }

  def getYAxisCount(c: Characteristic): Int = {
    getAxisCount(c, 1)
  }

  private def getAxisCount(c: Characteristic, i: Int): Int = {
    val xAxisDescr = c.getAxisDescriptions.asScala(i)

    Option(xAxisDescr.getAxisPoints_ref) match
      case Some(xAxisRef) =>
        require(
          xAxisDescr.getMaxAxisPoints == axisPts(xAxisRef).getMaxAxisPoints,
          s"Mismatched axis points count for ${c.getName}"
        )
        axisPts(xAxisRef).getMaxAxisPoints.toInt
      case None =>
        xAxisDescr.getMaxAxisPoints.toInt
  }

  private def collectWithPf[T: ClassTag](id: T => String): Map[String, T] =
    a2l
      .iterator()
      .asScala
      .collect { case t: T => t }
      .map(n => id(n) -> n)
      .toMap
}

object A2LWrapper {

  def getDecimalPlaces(c: Characteristic): Int = {
    getFormat(c.getFormat)._2
  }

  def getDecimalPlaces(axisDescr: AxisDescr): Int = {
    getFormat(axisDescr.getFormat)._2
  }

  def getDecimalPlaces(a: AxisPts): Int = {
    getFormat(a.getFormat)._2
  }

  private def getFormat(c: Characteristic): (Int, Int) = {
    getFormat(c.getFormat)
  }

  private def getFormat(axisDescr: AxisDescr): (Int, Int) = {
    getFormat(axisDescr.getFormat)
  }

  private def getFormat(a: AxisPts): (Int, Int) = {
    getFormat(a.getFormat)
  }

  private val formatExpr = """%(\d+)\.(\d+)""".r

  private def getFormat(format: String): (Int, Int) = {
    format match
      case formatExpr(len, dp) =>
        (len.toInt, dp.toInt)
  }

  private def getA2L(a2lUrl: URL): Asap2File = {
    Using.resource(a2lUrl.openStream()) { i =>
      val parser: Asap2Parser = new Asap2Parser(i)
      println(s"PERFORMING A PARSE: $a2lUrl")
      parser.parse()
    }
  }

  def characteristicFold[T](
      c: Characteristic,
      fValue: Characteristic => T,
      fValBlk: Characteristic => T,
      fCurve: Characteristic => T,
      fMap: Characteristic => T
  ): T = {
    if (c.getType == VALUE)
      fValue(c)
    else if (c.getType == CURVE)
      fCurve(c)
    else if (c.getType == MAP)
      fMap(c)
    else if (c.getType == VAL_BLK)
      fValBlk(c)
    else
      throw new IllegalStateException(s"Unsupported characteristic type: ${c.getType}")
  }

  def getObjectDescription(name: String, default: String): String = {
    BmwTchDescriptions.table.getOrElse(name, default)
  }

  private def getType(r: RecordLayout): DataType = {
    Option(r.getFunctionValues).map(_.getDataType).getOrElse(r.getAxisPtsX.getDatatype)
  }
}
