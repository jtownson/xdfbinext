package net.jtownson.xdfbinext

import net.alenzen.a2l.*
import net.alenzen.a2l.enums.CharacteristicType.{CURVE, MAP, VALUE, VAL_BLK}
import net.alenzen.a2l.enums.{CharacteristicType, ConversionType}
import net.jtownson.xdfbinext.A2LBinAdapter.CharacteristicValue
import net.jtownson.xdfbinext.a2l.CurveType.*
import net.jtownson.xdfbinext.a2l.MapType.*
import net.jtownson.xdfbinext.a2l.ValueConsumer.ValueType
import net.jtownson.xdfbinext.a2l.ValBlkConsumer.ValueBlkType
import net.jtownson.xdfbinext.a2l.{CompuTab, CompuVTab, *}

import java.io.{File, RandomAccessFile}

/** What would be nice here would be to convert an a2l+bin to some kind of repr where we are able to answer questions
  * such as 'find axes where the units are kg/h and the values are below 1300 kg/h.
  */
class A2LBinAdapter(val bin: File, a2l: A2LWrapper, offset: Long = 0x9000000) {

  def readCharacteristic(cName: String): CharacteristicValue = {
    readCharacteristic(a2l.characteristics(cName))
  }

  def readValue(cName: String): String | BigDecimal = {
    readValue(a2l.characteristics(cName))
  }

  private def readValue(c: Characteristic): String | BigDecimal = {

    val valueConsumer = ValueConsumer(c, a2l.getRecordLayout(c), offset, binAccess)

    getFormula(c) match
      case cvt: CompuVTab =>
        valueConsumer.applyFuncVTab(cvt)

      case ct: CompuTab =>
        valueConsumer.applyFuncTab(ct)

      case ratFun: RatFun =>
        valueConsumer.applyFuncFormula(ratFun, A2LWrapper.getDecimalPlaces(c))
  }

  private def readValBlk(c: Characteristic): ValueBlkType = {

    val consumer = ValBlkConsumer(c, a2l.getRecordLayout(c), offset, binAccess)

    getFormula(c) match
      case cvt: CompuVTab =>
        consumer.applyFuncVTab(cvt)
      case ct: CompuTab =>
        consumer.applyFuncTab(ct)
      case rf: RatFun =>
        consumer.applyFuncFormula(rf, A2LWrapper.getDecimalPlaces(c))
      case _ =>
        ???

  }

  def readCurve(cName: String): CurveValueType = {
    readCurve(a2l.characteristics(cName))
  }

  private def compuMethodCata1D(
      axisCompu: CompuMethodType,
      axDp: => Int,
      fnCompu: CompuMethodType,
      fnDp: => Int,
      consumer: CurveConsumer
  ): CurveValueType = {

    val xApp = compuMethodCata(
      rf => consumer.applyAxisFormula(rf, axDp),
      vt => consumer.applyAxisVTab(vt),
      ct => consumer.applyAxisTab(ct)
    )(axisCompu)

    val fApp: NumericArray | StringArray = compuMethodCata(
      rf => consumer.applyFuncFormula(rf, fnDp),
      vt => consumer.applyFuncVTab(vt),
      ct => consumer.applyFuncTab(ct)
    )(fnCompu)

    (xApp, fApp) match
      case (NumericArray(x), NumericArray(f)) =>
        NumberNumberTable1D(x, f)
      case (NumericArray(x), StringArray(f)) =>
        NumberStringTable1D(x, f)
      case (StringArray(x), NumericArray(f)) =>
        StringNumberTable1D(x, f)
      case (StringArray(x), StringArray(f)) =>
        StringStringTable1D(x, f)
  }

  private def compuMethodCata2D(
      xAxisCompu: CompuMethodType,
      xDp: => Int,
      yAxisCompu: CompuMethodType,
      yDp: => Int,
      fnCompu: CompuMethodType,
      fnDp: => Int,
      consumer: MapConsumer
  ): MapValueType = {

    val xApp = compuMethodCata(
      rf => consumer.applyXAxisFormula(rf, xDp),
      vt => consumer.applyXAxisVTab(vt),
      ct => consumer.applyXAxisTab(ct)
    )(xAxisCompu)

    val yApp = compuMethodCata(
      rf => consumer.applyYAxisFormula(rf, yDp),
      vt => consumer.applyYAxisVTab(vt),
      ct => consumer.applyYAxisTab(ct)
    )(yAxisCompu)

    val fApp = compuMethodCata(
      rf => consumer.applyFuncFormula(rf, fnDp),
      vt => consumer.applyFuncVTab(vt),
      ct => consumer.applyFuncTab(ct)
    )(fnCompu)

    (xApp, yApp, fApp) match {
      case (NumericArray(x), NumericArray(y), NumericArray(z)) =>
        NumberNumberNumberTable2D(x, y, z)

      case (NumericArray(x), NumericArray(y), StringArray(z)) =>
        NumberNumberStringTable2D(x, y, z)

      case (NumericArray(x), StringArray(y), NumericArray(z)) =>
        NumberStringNumberTable2D(x, y, z)

      case (NumericArray(x), StringArray(y), StringArray(z)) =>
        NumberStringStringTable2D(x, y, z)

      case (StringArray(x), NumericArray(y), StringArray(z)) =>
        StringNumberStringTable2D(x, y, z)

      case (StringArray(x), NumericArray(y), NumericArray(z)) =>
        StringNumberNumberTable2D(x, y, z)

      case (StringArray(x), StringArray(y), StringArray(z)) =>
        StringStringStringTable2D(x, y, z)

      case (StringArray(x), StringArray(y), NumericArray(z)) =>
        StringStringNumberTable2D(x, y, z)
    }
  }

  def readCurve(c: Characteristic): CurveValueType = {
    val fnRecordLayout = a2l.getRecordLayout(c)
    val fnFormula      = getFormula(c)

    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref) match
      case Some(axisPtsRef) =>
        val axisPts          = a2l.getXAxisPts(c)
        val axisType         = a2l.getType(axisPts)
        val axisRecordLayout = a2l.getRecordLayout(axisPts)

        val consumer = CurveConsumer(c, axisType, axisPts, axisRecordLayout, fnRecordLayout, offset, binAccess)

        val axisCompu = getFormula(axisPts)

        compuMethodCata1D(
          axisCompu,
          A2LWrapper.getDecimalPlaces(axisPts),
          fnFormula,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )

      case None =>
        val axisDesc  = c.getAxisDescriptions.get(0)
        val consumer  = CurveConsumer(c, axisDesc, fnRecordLayout, offset, binAccess)
        val axisCompu = getFormula(axisDesc)

        compuMethodCata1D(
          axisCompu,
          A2LWrapper.getDecimalPlaces(axisDesc),
          fnFormula,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )
  }

  def readMap(cName: String): MapValueType = {
    readMap(a2l.characteristics(cName))
  }

  def readMap(c: Characteristic): MapValueType = {
    val fnRecordLayout = a2l.getRecordLayout(c)
    val fnCompuMethod  = getFormula(c)

    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref)
      .and(Option(c.getAxisDescriptions.get(1).getAxisPoints_ref)) match {
      case Some((xAxisPtsRef, yAxisPtsRef)) =>
        val xAxisPts          = a2l.getXAxisPts(c)
        val xAxisFormat       = xAxisPts.getFormat
        val xAxisCompu        = getFormula(xAxisPts)
        val xAxisType         = a2l.getType(xAxisPts)
        val xAxisRecordLayout = a2l.getRecordLayout(xAxisPts)

        val yAxisPts          = a2l.getYAxisPts(c)
        val yAxisFormat       = yAxisPts.getFormat
        val yAxisCompu        = getFormula(yAxisPts)
        val yAxisType         = a2l.getType(yAxisPts)
        val yAxisRecordLayout = a2l.getRecordLayout(yAxisPts)

        val consumer = MapConsumer(
          c,
          xAxisType,
          xAxisPts,
          xAxisRecordLayout,
          yAxisType,
          yAxisPts,
          yAxisRecordLayout,
          fnRecordLayout,
          offset,
          binAccess
        )

        compuMethodCata2D(
          xAxisCompu,
          A2LWrapper.getDecimalPlaces(xAxisPts),
          yAxisCompu,
          A2LWrapper.getDecimalPlaces(yAxisPts),
          fnCompuMethod,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )

      case None =>
        val xAxisDesc   = c.getAxisDescriptions.get(0)
        val xAxisFormat = xAxisDesc.getFormat
        val xAxisCompu  = getFormula(xAxisDesc)

        val yAxisDesc   = c.getAxisDescriptions.get(1)
        val yAxisFormat = yAxisDesc.getFormat
        val yAxisCompu  = getFormula(yAxisDesc)

        val consumer = MapConsumer(c, xAxisDesc, yAxisDesc, fnRecordLayout, offset, binAccess)

        compuMethodCata2D(
          xAxisCompu,
          A2LWrapper.getDecimalPlaces(xAxisDesc),
          yAxisCompu,
          A2LWrapper.getDecimalPlaces(yAxisDesc),
          fnCompuMethod,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )
    }
  }

  private def readCharacteristic(c: Characteristic): CharacteristicValue = {
    if (c.getType == VALUE) {
      readValue(c)
    } else if (c.getType == CURVE) {
      readCurve(c)
    } else if (c.getType == MAP) {
      readMap(c)
    } else if (c.getType == VAL_BLK) {
      readValBlk(c)
    } else {
      ???
    }
  }

  private def getFormula(c: Characteristic): CompuMethodType = {
    val compuMethod = a2l.compuMethods(c.getConversion)
    getFormula(compuMethod)
  }

  private def getFormula(a: AxisPts): CompuMethodType = {
    val compuMethod = a2l.compuMethods(a.getConversion)
    getFormula(compuMethod)
  }

  private def getFormula(a: AxisDescr): CompuMethodType = {
    val compuMethod = a2l.compuMethods(a.getConversion)
    getFormula(compuMethod)
  }

  import scala.jdk.CollectionConverters.*

  private def getFormula(compuMethod: CompuMethod): CompuMethodType = {
    val conversionType = compuMethod.getConversionType
    if (conversionType == ConversionType.RAT_FUNC) {
      val coeffs = compuMethod.getCoeffs
      RatFun(coeffs.getA, coeffs.getB, coeffs.getC, coeffs.getD, coeffs.getE, coeffs.getF)
    } else if (conversionType == ConversionType.TAB_VERB) {
      val entries = a2l
        .compuVTabs(compuMethod.getCompuTab_ref)
        .getValuePairs
        .asScala
        .map(vp => vp.getInVal.toInt -> vp.getOutVal)
        .toMap

      CompuVTab(entries)

    } else if (conversionType == ConversionType.TAB_INTP || conversionType == ConversionType.TAB_NOINTP) {
      val entries = a2l
        .compuTabs(compuMethod.getCompuTab_ref)
        .getValuePairs
        .asScala
        .map(vp => BigDecimal(vp.getInVal) -> BigDecimal(vp.getOutVal))

      val x  = entries.map(_._1).toArray
      val fx = entries.map(_._2).toArray

      CompuTab(x, fx)
    } else {
      ???
    }
  }

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

}

object A2LBinAdapter {

  type CharacteristicValue = ValueType | ValueBlkType | CurveValueType | MapValueType

}
