package net.jtownson.xdfbinext

import cats.kernel.Monoid
import net.alenzen.a2l.enums.CharacteristicType.{ASCII, CURVE, MAP, VALUE, VAL_BLK}
import net.alenzen.a2l.enums.{CharacteristicType, ConversionType, DataType}
import net.alenzen.a2l.*
import net.jtownson.xdfbinext.A2LWrapper.{characteristicFold, compuMethodTypeFold, getA2L, getObjectDescription}
import net.jtownson.xdfbinext.a2l.{CharacteristicSummary, CompuMethodType, RatFun}
import net.jtownson.xdfbinext.a2l.CharacteristicSummary.{
  AsciiSummary,
  CurveSummary,
  MapSummary,
  ValBlkSummary,
  ValueSummary
}

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Using

case class A2LWrapper(a2lUrl: URL) {

  val a2l: Asap2File = getA2L(a2lUrl)

  def memorySegmentsOrdered: Iterator[MemorySegment]   = collectOrderedWithPf[MemorySegment]
  def recordLayoutsOrdered: Iterator[RecordLayout]     = collectOrderedWithPf[RecordLayout]
  def compuMethodsOrdered: Iterator[CompuMethod]       = collectOrderedWithPf[CompuMethod]
  def compuVTabsOrdered: Iterator[CompuVTab]           = collectOrderedWithPf[CompuVTab]
  def compuTabsOrdered: Iterator[CompuTab]             = collectOrderedWithPf[CompuTab]
  def characteristicsOrdered: Iterator[Characteristic] = collectOrderedWithPf[Characteristic]
  def measurementsOrdered: Iterator[Measurement]       = collectOrderedWithPf[Measurement]
  def axisPtsOrdered: Iterator[AxisPts]                = collectOrderedWithPf[AxisPts]
  def functionsOrdered: Iterator[Function]             = collectOrderedWithPf[Function]

  val memorySegments: Map[String, MemorySegment] = collectWithPf[MemorySegment](_.getName)

  private val memorySegmentsByAddress: List[Long] = memorySegments.values.map(_.getAddress).toList.sorted

  def segmentForAddress(address: Long): Long = {
    memorySegmentsByAddress.takeWhile(segmentAddress => segmentAddress <= address).last
  }

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

  def getFormula(c: Characteristic): CompuMethodType = {
    val compuMethod = compuMethods(c.getConversion)
    getFormula(compuMethod)
  }

  def getFormula(a: AxisPts): CompuMethodType = {
    val compuMethod = compuMethods(a.getConversion)
    getFormula(compuMethod)
  }

  def getFormula(a: AxisDescr): CompuMethodType = {
    val compuMethod = compuMethods(a.getConversion)
    getFormula(compuMethod)
  }

  def getFormula(compuMethod: CompuMethod): CompuMethodType = {
    val conversionType = compuMethod.getConversionType
    if (conversionType == ConversionType.RAT_FUNC) {
      val coeffs = compuMethod.getCoeffs
      RatFun(coeffs.getA, coeffs.getB, coeffs.getC, coeffs.getD, coeffs.getE, coeffs.getF)
    } else if (conversionType == ConversionType.TAB_VERB) {
      val entries = compuVTabs(compuMethod.getCompuTab_ref).getValuePairs.asScala
        .map(vp => vp.getInVal.toInt -> vp.getOutVal)
        .toMap

      net.jtownson.xdfbinext.a2l.CompuVTab(entries)

    } else if (conversionType == ConversionType.TAB_INTP || conversionType == ConversionType.TAB_NOINTP) {
      val entries = compuTabs(compuMethod.getCompuTab_ref).getValuePairs.asScala
        .map(vp => BigDecimal(vp.getInVal) -> BigDecimal(vp.getOutVal))

      val x  = entries.map(_._1).toArray
      val fx = entries.map(_._2).toArray

      net.jtownson.xdfbinext.a2l.CompuTab(x, fx)
    } else {
      ???
    }
  }

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

    def asciiSummary(c: Characteristic) =
      AsciiSummary(
        c.getName,
        getObjectDescription(c.getName, c.getLongIdentifier),
        characteristicUsage(c.getName)
      )

    characteristicFold(c, valueSummary, valBlkSummary, curveSummary, mapSummary, asciiSummary)

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

  def getFormat(m: Measurement): (Int, Int) = {
    val f = m.getFormat
    Option(f).map(A2LWrapper.getFormat).getOrElse(getFormat(compuMethods(m.getConversion)))
  }

  def getFormat(c: CompuMethod): (Int, Int) = {
    A2LWrapper.getFormat(c.getFormat)
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

  private def collectOrderedWithPf[T: ClassTag]: Iterator[T] = {
    val i = a2l
      .iterator()
      .asScala
      .collect { case t: T => t }

    i
  }

  def valueTypeFold[T](c: Characteristic, fString: () => T, fNumber: () => T): T = {
    compuMethodTypeFold(getFormula(c), fString, fNumber)
  }

  def getXAxisFormula(c: Characteristic): CompuMethodType = {
    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref) match
      case Some(axisPtsRef) =>
        val axisPts = getXAxisPts(c)
        getFormula(axisPts)

      case None =>
        val axisDesc = c.getAxisDescriptions.get(0)
        getFormula(axisDesc)
  }

  def getYAxisFormula(c: Characteristic): CompuMethodType = {
    Option(c.getAxisDescriptions.get(1).getAxisPoints_ref) match
      case Some(axisPtsRef) =>
        val axisPts = getYAxisPts(c)
        getFormula(axisPts)

      case None =>
        val axisDesc = c.getAxisDescriptions.get(1)
        getFormula(axisDesc)
  }

  def curveTypeFold[T](
      c: Characteristic,
      fNumberString: () => T,
      fNumberNumber: () => T,
      fStringNumber: () => T,
      fStringString: () => T
  ): T = {
    val fnFormula   = getFormula(c)
    val axisFormula = getXAxisFormula(c)

    compuMethodTypeFold(
      axisFormula,
      fString = () => compuMethodTypeFold(fnFormula, fString = fStringString, fNumber = fStringNumber),
      fNumber = () => compuMethodTypeFold(fnFormula, fString = fNumberString, fNumber = fNumberNumber)
    )
  }

  def mapTypeFold[T](
      c: Characteristic,
      fNumberNumberNumber: () => T,
      fNumberNumberString: () => T,
      fNumberStringNumber: () => T,
      fNumberStringString: () => T,
      fStringNumberString: () => T,
      fStringNumberNumber: () => T,
      fStringStringString: () => T,
      fStringStringNumber: () => T
  ): T = {
    val fnRecordLayout = getRecordLayout(c)
    val fnFormula      = getFormula(c)
    val xAxisFormula   = getXAxisFormula(c)
    val yAxisFormula   = getYAxisFormula(c)

    compuMethodTypeFold(
      xAxisFormula,
      fString = () =>
        compuMethodTypeFold(
          yAxisFormula,
          fString = () => compuMethodTypeFold(fnFormula, fString = fStringStringString, fNumber = fStringStringNumber),
          fNumber = () => compuMethodTypeFold(fnFormula, fString = fStringNumberString, fNumber = fStringNumberNumber)
        ),
      fNumber = () =>
        compuMethodTypeFold(
          yAxisFormula,
          fString = () => compuMethodTypeFold(fnFormula, fString = fNumberStringString, fNumber = fNumberStringNumber),
          fNumber = () => compuMethodTypeFold(fnFormula, fString = fNumberNumberString, fNumber = fNumberNumberNumber)
        )
    )
  }

  def characteristicTypeFold[T](
      c: Characteristic,
      fString: () => T,
      fNumber: () => T,
      fStringArr: () => T,
      fNumberArr: () => T,
      fNumberString: () => T,
      fNumberNumber: () => T,
      fStringNumber: () => T,
      fStringString: () => T,
      fNumberNumberNumber: () => T,
      fNumberNumberString: () => T,
      fNumberStringNumber: () => T,
      fNumberStringString: () => T,
      fStringNumberString: () => T,
      fStringNumberNumber: () => T,
      fStringStringString: () => T,
      fStringStringNumber: () => T
  ): T = {

    if (c.getType == VALUE)
      compuMethodTypeFold(getFormula(c), fString, fNumber)
    else if (c.getType == CURVE)
      curveTypeFold(c, fNumberString, fNumberNumber, fStringNumber, fStringString)
    else if (c.getType == MAP)
      mapTypeFold(
        c,
        fNumberNumberNumber,
        fNumberNumberString,
        fNumberStringNumber,
        fNumberStringString,
        fStringNumberString,
        fStringNumberNumber,
        fStringStringString,
        fStringStringNumber
      )
    else if (c.getType == VAL_BLK) {
      compuMethodTypeFold(getFormula(c), fStringArr, fNumberArr)
    } else if (c.getType == ASCII)
      fString()
    else
      throw new IllegalStateException(s"Unsupported characteristic type: ${c.getType}")
  }

  def saneCharacteristicTypeFold[T: Monoid](cName: String): SaneCharacteristicTypeFold[T] =
    new SaneCharacteristicTypeFold(characteristics(cName))

  def saneCharacteristicTypeFold[T: Monoid](c: Characteristic): SaneCharacteristicTypeFold[T] =
    new SaneCharacteristicTypeFold(c)

  class SaneCharacteristicTypeFold[T](c: Characteristic)(implicit ev: Monoid[T]) {

    def withDefaultArgs(
        fString: () => T = () => Monoid.empty[T],
        fNumber: () => T = () => Monoid.empty[T],
        fStringArr: () => T = () => Monoid.empty[T],
        fNumberArr: () => T = () => Monoid.empty[T],
        fNumberString: () => T = () => Monoid.empty[T],
        fNumberNumber: () => T = () => Monoid.empty[T],
        fStringNumber: () => T = () => Monoid.empty[T],
        fStringString: () => T = () => Monoid.empty[T],
        fNumberNumberNumber: () => T = () => Monoid.empty[T],
        fNumberNumberString: () => T = () => Monoid.empty[T],
        fNumberStringNumber: () => T = () => Monoid.empty[T],
        fNumberStringString: () => T = () => Monoid.empty[T],
        fStringNumberString: () => T = () => Monoid.empty[T],
        fStringNumberNumber: () => T = () => Monoid.empty[T],
        fStringStringString: () => T = () => Monoid.empty[T],
        fStringStringNumber: () => T = () => Monoid.empty[T]
    ): T = {
      characteristicTypeFold(
        c: Characteristic,
        fString,
        fNumber,
        fStringArr,
        fNumberArr,
        fNumberString,
        fNumberNumber,
        fStringNumber,
        fStringString,
        fNumberNumberNumber,
        fNumberNumberString,
        fNumberStringNumber,
        fNumberStringString,
        fStringNumberString,
        fStringNumberNumber,
        fStringStringString,
        fStringStringNumber
      )
    }
  }
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

  def getFormat(format: String): (Int, Int) = {
    format match
      case formatExpr(len, dp) =>
        (len.toInt, dp.toInt)
  }

  def getA2L(a2lUrl: URL): Asap2File = {
    Using.resource(a2lUrl.openStream()) { i =>
      val parser: Asap2Parser = new Asap2Parser(i)
      parser.parse()
    }
  }

  def characteristicFold[T](
      c: Characteristic,
      fValue: Characteristic => T,
      fValBlk: Characteristic => T,
      fCurve: Characteristic => T,
      fMap: Characteristic => T,
      fAscii: Characteristic => T
  ): T = {
    if (c.getType == VALUE)
      fValue(c)
    else if (c.getType == CURVE)
      fCurve(c)
    else if (c.getType == MAP)
      fMap(c)
    else if (c.getType == VAL_BLK)
      fValBlk(c)
    else if (c.getType == ASCII)
      fAscii(c)
    else
      throw new IllegalStateException(s"Unsupported characteristic type: ${c.getType}")
  }

  def getObjectDescription(name: String, default: String): String = {
    BmwTchDescriptions.table.getOrElse(name, default)
  }

  def getType(r: RecordLayout): DataType = {
    Option(r.getFunctionValues).map(_.getDataType).getOrElse(r.getAxisPtsX.getDatatype)
  }

  def compuMethodTypeFold[T](c: CompuMethodType, fString: () => T, fNumber: () => T): T = {
    c match {
      case cvt: net.jtownson.xdfbinext.a2l.CompuVTab =>
        fString()

      case ct: net.jtownson.xdfbinext.a2l.CompuTab =>
        fNumber()

      case ratFun: net.jtownson.xdfbinext.a2l.RatFun =>
        fNumber()
    }
  }
}
