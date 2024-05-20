package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.CharacteristicType.{CURVE, MAP, VALUE, VAL_BLK}
import net.alenzen.a2l.enums.DataType.*
import net.alenzen.a2l.enums.{CharacteristicType, ConversionType, DataType}
import net.alenzen.a2l.*
import net.jtownson.xdfbinext.A2LBinAdapter.{CharacteristicValue, CompuMethodType}
import net.jtownson.xdfbinext.a2l.{CompuTab, CompuVTab, *}
import net.jtownson.xdfbinext.a2l.CurveType.*
import net.jtownson.xdfbinext.a2l.MapType.{MapValueType, NumberNumberNumberTable2D, NumberNumberStringTable2D}
import net.jtownson.xdfbinext.a2l.PositionWrapper.{AxPtsPosition, FncValuesPosition, FncValuesPositionXY, NoAxPtsPosition}
import net.jtownson.xdfbinext.a2l.RatFunFormula.RatFun
import net.jtownson.xdfbinext.a2l.{RecordConsumer1D, RecordConsumer2D, StringValBlk, ValueConsumer}

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.HALF_UP

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
    getFormula(c) match
      case cvt: CompuVTab =>
        ValueConsumer(a2l.getType(c), offsetAddress(c), 1, binAccess).applyVTab(cvt).head

      case ct: CompuTab =>
        ValueConsumer(a2l.getType(c), offsetAddress(c), 1, binAccess).applyTab(ct).head

      case ratFun: RatFun =>
        val dp = A2LWrapper.getDecimalPlaces(c)
        ValueConsumer(a2l.getType(c), offsetAddress(c), 1, binAccess).applyFormula(ratFun, dp).head
  }

  private def readValBlk(c: Characteristic): StringValBlk = {
    val cellCount = a2l.getCellCount(c)
    val tpe       = a2l.getType(c)

    getFormula(c) match
      case cvt: CompuVTab =>
        val values = ValueConsumer(a2l.getType(c), offsetAddress(c), cellCount, binAccess).applyVTab(cvt)
        StringValBlk(values: _*)
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
      consumer: RecordConsumer1D
  ): CurveValueType = {
    (axisCompu, fnCompu) match
      case (arf: RatFun, frf: RatFun) =>
        NumberNumberTable1D(consumer.applyAxisFormula(arf, axDp), consumer.applyFuncFormula(frf, fnDp))
      case (arf: RatFun, frf: CompuVTab) =>
        NumberStringTable1D(consumer.applyAxisFormula(arf, axDp), consumer.applyFuncVTab(frf))
      case (arf: CompuVTab, frf: RatFun) =>
        StringNumberTable1D(consumer.applyAxisVTab(arf), consumer.applyFuncFormula(frf, fnDp))
      case (arf: CompuVTab, frf: CompuVTab) =>
        StringStringTable1D(consumer.applyAxisVTab(arf), consumer.applyFuncVTab(frf))
  }

  private def compuMethodCata2D(
      xAxisCompu: CompuMethodType,
      xDp: => Int,
      yAxisCompu: CompuMethodType,
      yDp: => Int,
      fnCompu: CompuMethodType,
      fnDp: => Int,
      consumer: RecordConsumer2D
  ): MapValueType = {
    (xAxisCompu, yAxisCompu, fnCompu) match {
      case (xrf: RatFun, yrf: RatFun, frf: RatFun) =>
        NumberNumberNumberTable2D(
          consumer.applyXAxisFormula(xrf, xDp),
          consumer.applyYAxisFormula(yrf, yDp),
          consumer.applyFuncFormula(frf, fnDp)
        )
      case (xrf: RatFun, yrf: RatFun, fvt: CompuVTab) =>
        NumberNumberStringTable2D(
          consumer.applyXAxisFormula(xrf, xDp),
          consumer.applyYAxisFormula(yrf, yDp),
          consumer.applyFuncVTab(fvt)
        )
    }
  }

  def readCurve(c: Characteristic): CurveValueType = {
    val fnRecordLayout = a2l.getRecordLayout(c)
    val fnFormula      = getFormula(c)

    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref) match
      case Some(axisPtsRef) =>
        val axisPts          = a2l.getXAxisPts(c)
        val axisType         = a2l.getType(axisPts)
        val axisFormat       = axisPts.getFormat
        val axisRecordLayout = a2l.getRecordLayout(axisPts)

        val consumer = RecordConsumer1D(c, axisType, axisPts, axisRecordLayout, fnRecordLayout, offset, binAccess)

        val axisCompu = getFormula(axisPts)

        compuMethodCata1D(
          axisCompu,
          A2LWrapper.getDecimalPlaces(axisPts),
          fnFormula,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )

      case None =>
        val axisDesc   = c.getAxisDescriptions.get(0)
        val axisFormat = axisDesc.getFormat
        val consumer   = RecordConsumer1D(c, axisDesc, fnRecordLayout, offset, binAccess)
        val axDp       = A2LWrapper.getDecimalPlaces(axisDesc)
        val fnDp       = A2LWrapper.getDecimalPlaces(c)
        val axisCompu  = getFormula(axisDesc)

        compuMethodCata1D(axisCompu, axDp, fnFormula, fnDp, consumer)
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

        val consumer = RecordConsumer2D(
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

        val consumer = RecordConsumer2D(c, xAxisDesc, yAxisDesc, fnRecordLayout, offset, binAccess)

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

  private def offsetAddress(c: Characteristic): Long =
    c.getAddress - offset

  private def offsetAddress(a: AxisPts): Long =
    a.getAddress - offset

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

}

object A2LBinAdapter {

  type CharacteristicValue = String | BigDecimal | StringValBlk | CurveValueType | MapValueType

  type CompuMethodType = RatFun | CompuVTab | CompuTab

}
