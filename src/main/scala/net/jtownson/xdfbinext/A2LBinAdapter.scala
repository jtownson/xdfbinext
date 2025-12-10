package net.jtownson.xdfbinext

import net.alenzen.a2l.*
import net.alenzen.a2l.enums.CharacteristicType.{ASCII, CURVE, MAP, VALUE, VAL_BLK}
import net.alenzen.a2l.enums.{CharacteristicType, ConversionType}
import net.jtownson.xdfbinext.A2LBinAdapter.CharacteristicValue
import net.jtownson.xdfbinext.a2l.CurveType.{CurveValueType, *}
import net.jtownson.xdfbinext.a2l.MapType.{MapValueType, NumberNumberStringTable2D, NumberStringNumberTable2D, *}
import net.jtownson.xdfbinext.a2l.ValueConsumer.ValueType
import net.jtownson.xdfbinext.a2l.ValBlkConsumer.ValBlkType
import net.jtownson.xdfbinext.a2l.{CompuTab, CompuVTab, MapType, *}

import scala.jdk.CollectionConverters.*
import java.io.{File, RandomAccessFile}

/** What would be nice here would be to convert an a2l+bin to some kind of repr where we are able to answer questions
  * such as _find axes where the units are kg/h and the values are below 1300 kg/h_.
  */
class A2LBinAdapter(val bin: File, val a2l: A2LWrapper, offset: Long = 0x9000000) {

  // TODO this handle is leaked
  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

  def numberNumberNumberTable2D(name: String): NumberNumberNumberTable2D =
    readMap(name).asInstanceOf[NumberNumberNumberTable2D]

  def numberNumberTable1D(name: String): NumberNumberTable1D =
    readCurve(name).asInstanceOf[NumberNumberTable1D]

  case class BaceMap(a2LBinAdapter: A2LBinAdapter, name: String) {
    private val mapValue: MapValueType = a2LBinAdapter.readMap(name)

    def apply(x: BigDecimal, y: BigDecimal): BigDecimal = mapValue match {
      case m: NumberNumberNumberTable2D =>
        m.atXY(x, y)
      case x =>
        throw new UnsupportedOperationException()
    }
  }

  def readCharacteristicWithCast[T](cName: String): T = {
    readCharacteristic(a2l.characteristics(cName)).asInstanceOf[T]
  }

  def readCharacteristic(cName: String): CharacteristicValue = {
    readCharacteristic(a2l.characteristics(cName))
  }

  private def readValue(cName: String): String | BigDecimal = {
    readValue(a2l.characteristics(cName))
  }

  private def readAscii(c: Characteristic): String = {
    val t  = a2l.getType(c)
    val n  = c.getNumber.toInt
    val cm = a2l.getCompuMethod(c)
    val rl = a2l.getRecordLayout(c)
    BlockConsumer.readUByte(c.getAddress - offset, n, binAccess).takeWhile(_ != 0).map(_.toChar).mkString
  }

  private def readValue(c: Characteristic): String | BigDecimal = {

    val valueConsumer = ValueConsumer(c, a2l.getRecordLayout(c), offset, binAccess)

    a2l.getFormula(c) match
      case cvt: CompuVTab =>
        valueConsumer.applyFuncVTab(cvt)

      case ct: CompuTab =>
        valueConsumer.applyFuncTab(ct)

      case ratFun: RatFun =>
        valueConsumer.applyFuncFormula(ratFun, A2LWrapper.getDecimalPlaces(c))
  }

  private def readValBlk(c: Characteristic): ValBlkType = {

    val consumer = ValBlkConsumer(c, a2l.getRecordLayout(c), offset, binAccess)

    a2l.getFormula(c) match
      case cvt: CompuVTab =>
        consumer.applyFuncVTab(cvt)
      case ct: CompuTab =>
        consumer.applyFuncTab(ct)
      case rf: RatFun =>
        consumer.applyFuncFormula(rf, A2LWrapper.getDecimalPlaces(c))
  }

  private def readCurve(cName: String): CurveValueType = {
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

  private def readCurve(c: Characteristic): CurveValueType = {
    val fnRecordLayout = a2l.getRecordLayout(c)
    val fnFormula      = a2l.getFormula(c)

    val axisCompu = a2l.getXAxisFormula(c)

    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref) match
      case Some(axisPtsRef) =>
        val axisPts          = a2l.getXAxisPts(c)
        val axisType         = a2l.getType(axisPts)
        val axisRecordLayout = a2l.getRecordLayout(axisPts)

        val consumer = CurveConsumer(c, axisType, axisPts, axisRecordLayout, fnRecordLayout, offset, binAccess)

        compuMethodCata1D(
          axisCompu,
          A2LWrapper.getDecimalPlaces(axisPts),
          fnFormula,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )

      case None =>
        val axisDesc = c.getAxisDescriptions.get(0)
        val consumer = CurveConsumer(c, axisDesc, fnRecordLayout, offset, binAccess)

        compuMethodCata1D(
          axisCompu,
          A2LWrapper.getDecimalPlaces(axisDesc),
          fnFormula,
          A2LWrapper.getDecimalPlaces(c),
          consumer
        )
  }

  private def readMap(cName: String): MapValueType = {
    readMap(a2l.characteristics(cName))
  }

  private def readMap(c: Characteristic): MapValueType = {
    val fnRecordLayout = a2l.getRecordLayout(c)
    val fnCompuMethod  = a2l.getFormula(c)

    Option(c.getAxisDescriptions.get(0).getAxisPoints_ref)
      .and(Option(c.getAxisDescriptions.get(1).getAxisPoints_ref)) match {
      case Some((xAxisPtsRef, yAxisPtsRef)) =>
        val xAxisPts          = a2l.getXAxisPts(c)
        val xAxisFormat       = xAxisPts.getFormat
        val xAxisCompu        = a2l.getFormula(xAxisPts)
        val xAxisType         = a2l.getType(xAxisPts)
        val xAxisRecordLayout = a2l.getRecordLayout(xAxisPts)

        val yAxisPts          = a2l.getYAxisPts(c)
        val yAxisFormat       = yAxisPts.getFormat
        val yAxisCompu        = a2l.getFormula(yAxisPts)
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
        val xAxisCompu  = a2l.getFormula(xAxisDesc)

        val yAxisDesc   = c.getAxisDescriptions.get(1)
        val yAxisFormat = yAxisDesc.getFormat
        val yAxisCompu  = a2l.getFormula(yAxisDesc)

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
    } else if (c.getType == ASCII) {
      readAscii(c)
    } else {
      ???
    }
  }

}

object A2LBinAdapter {
  type CharacteristicValue = ValueType | ValBlkType | CurveValueType | MapValueType

  def diffCharacteristic(lhs: CharacteristicValue, rhs: CharacteristicValue): CharacteristicValue = {
    (lhs, rhs) match {
      case (l: ValueType, r: ValueType) =>
        diffValueTypes(l, r)
      case (l: ValBlkType, r: ValBlkType) =>
        diffValBlkTypes(l, r)
      case (l: CurveValueType, r: CurveValueType) =>
        diffCurveValueTypes(l, r)
      case (l: MapValueType, r: MapValueType) =>
        diffMapValueTypes(l, r)
      case _ =>
        throw new IllegalStateException(s"Attempt to diff characteristics of different types: $lhs, $rhs")
    }
  }

  private def diffValueTypes(lhs: ValueType, rhs: ValueType): ValueType = {
    (lhs, rhs) match {
      case (l: String, r: String) =>
        l.diff(r)
      case (l: BigDecimal, r: BigDecimal) =>
        l - r
    }
  }

  private def diffValBlkTypes(lhs: ValBlkType, rhs: ValBlkType): ValBlkType = {
    (lhs, rhs) match {
      case (l: NumericArray, r: NumericArray) =>
        NumericArray(l.values.zip(r.values).map((nl, nr) => nl - nr))
      case (l: StringArray, r: StringArray) =>
        StringArray(l.values.zip(r.values).map((sl, sr) => sl.diff(sr)))
    }
  }

  // TODO consider smarter, interpolated diff when axes are also different
  private def diffCurveValueTypes(lhs: CurveValueType, rhs: CurveValueType): CurveValueType = {
    (lhs, rhs) match {
      case (l: NumberStringTable1D, r: NumberStringTable1D) =>
        NumberStringTable1D(Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: StringNumberTable1D, r: StringNumberTable1D) =>
        StringNumberTable1D(Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
      case (l: StringStringTable1D, r: StringStringTable1D) =>
        StringStringTable1D(Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: NumberNumberTable1D, r: NumberNumberTable1D) =>
        NumberNumberTable1D(Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
    }
  }

  // TODO consider smarter, interpolated diff when axes are also different
  private def diffMapValueTypes(lhs: MapValueType, rhs: MapValueType): MapValueType = {
    (lhs, rhs) match {
      case (l: NumberNumberNumberTable2D, r: NumberNumberNumberTable2D) =>
        NumberNumberNumberTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
      case (l: NumberNumberStringTable2D, r: NumberNumberStringTable2D) =>
        NumberNumberStringTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: NumberStringNumberTable2D, r: NumberStringNumberTable2D) =>
        NumberStringNumberTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
      case (l: NumberStringStringTable2D, r: NumberStringStringTable2D) =>
        NumberStringStringTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: StringNumberStringTable2D, r: StringNumberStringTable2D) =>
        StringNumberStringTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: StringNumberNumberTable2D, r: StringNumberNumberTable2D) =>
        StringNumberNumberTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
      case (l: StringStringStringTable2D, r: StringStringStringTable2D) =>
        StringStringStringTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln diff rn))
      case (l: StringStringNumberTable2D, r: StringStringNumberTable2D) =>
        StringStringNumberTable2D(Array.empty, Array.empty, l.values.zip(r.values).map((ln, rn) => ln - rn))
    }
  }
}
