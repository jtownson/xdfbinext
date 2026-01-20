package net.jtownson.xdfbinext

import breeze.io.RandomAccessFile
import net.alenzen.a2l.MemorySegment.{MemoryType, PrgType}
import net.alenzen.a2l.{Characteristic, Measurement, MemorySegment}
import net.alenzen.a2l.enums.{CharacteristicType, DataType}
import net.alenzen.a2l.enums.CharacteristicType.*
import net.jtownson.xdfbinext.A2LBinAdapter.{
  CharacteristicValue,
  FileBackedSegment,
  SegmentIndex,
  buildFileBackedSegment,
  isVariableRAM
}
import net.jtownson.xdfbinext.a2l.*
import net.jtownson.xdfbinext.a2l.A2LMeasurement.BinMeasurement
import net.jtownson.xdfbinext.a2l.CurveType.*
import net.jtownson.xdfbinext.a2l.MapType.*
import net.jtownson.xdfbinext.a2l.ValBlkConsumer.ValBlkType
import net.jtownson.xdfbinext.a2l.ValueConsumer.ValueType

import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.FileAttribute
import scala.collection.immutable.TreeMap

/** Facade over a2l memory blocks to support reading characteristics and reading/writing measurements.
  */
class A2LBinAdapter(val calibrationBin: File, val a2l: A2LWrapper, offset: Long = 0x9000000) extends AutoCloseable {

  private val binAccess: RandomAccessFile = new RandomAccessFile(calibrationBin, "r")

  val ramSegments: SegmentIndex = SegmentIndex(
    a2l.memorySegmentsOrdered
      .filter(isVariableRAM)
      .map(buildFileBackedSegment)
      .toSeq
  )

  def measurement(mName: String): A2LMeasurement = {
    BinMeasurement(a2l.measurements(mName), this)
  }

  def writeMeasurement(bds: Array[BigDecimal], dataType: DataType, raf: RandomAccessFile, off: Long, len: Int): Unit = {
    raf.seek(off)
    dataType match {
      case DataType.UBYTE =>
        raf.writeUInt8(bds.map(_.toShort))
      case DataType.SBYTE =>
        raf.writeInt8(bds.map(_.toByte))
      case DataType.UWORD =>
        raf.writeUInt16(bds.map(_.toChar))
      case DataType.SWORD =>
        raf.writeInt16(bds.map(_.toShort))
      case DataType.ULONG =>
        raf.writeUInt32(bds.map(_.toLong))
      case DataType.SLONG =>
        raf.writeInt32(bds.map(_.toInt))
      case DataType.FLOAT32_IEEE =>
        raf.writeFloat(bds.map(_.toFloat))
      case DataType.FLOAT64_IEEE =>
        raf.writeDouble(bds.map(_.toDouble))
      case _ => throw new UnsupportedOperationException()
    }
  }

  def readMeasurement(dataType: DataType, raf: RandomAccessFile, off: Long, len: Int): Array[BigDecimal] = {
    raf.seek(off)
    dataType match {
      case DataType.UBYTE =>
        raf.readUInt8(len).map(BigDecimal(_))
      case DataType.SBYTE =>
        raf.readInt8(len).map(BigDecimal(_))
      case DataType.UWORD =>
        raf.readUInt16(len).map(BigDecimal(_))
      case DataType.SWORD =>
        raf.readInt16(len).map(BigDecimal(_))
      case DataType.ULONG =>
        raf.readUInt32(len).map(BigDecimal(_))
      case DataType.SLONG =>
        raf.readInt32(len).map(BigDecimal(_))
      case DataType.FLOAT32_IEEE =>
        raf.readFloat(len).map(BigDecimal(_))
      case DataType.FLOAT64_IEEE =>
        raf.readDouble(len).map(BigDecimal(_))
      case _ => throw new UnsupportedOperationException()
    }
  }

  private def applyRatFun(m: Measurement, bds: Array[BigDecimal]): Array[BigDecimal] = {
    val compuMethod = a2l.getFormula(m)
    compuMethodCata(rf => bds.map(rf.apply), _ => bds, _ => bds)(compuMethod)
  }

  private def applyRatFunInverse(m: Measurement, bds: Array[BigDecimal]): Array[BigDecimal] = {
    val compuMethod = a2l.getFormula(m)
    compuMethodCata(rf => bds.map(rf.applyInverse), _ => bds, _ => bds)(compuMethod)
  }

  private def getXDim(m: Measurement): Int = Option(m.getMatrixDim).map(_.getxDim.toInt).getOrElse(1)
  private def getYDim(m: Measurement): Int = Option(m.getMatrixDim).map(_.getyDim.toInt).getOrElse(1)
  private def getZDim(m: Measurement): Int = Option(m.getMatrixDim).map(_.getzDim.toInt).getOrElse(1)

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

  override def close(): Unit = {
    binAccess.close()
  }
}

object A2LBinAdapter {

  case class FileBackedSegment(segment: MemorySegment, file: RandomAccessFile) extends AutoCloseable {

    def getCheckedOffset(address: Long): Long = {
      val off = address - segment.getAddress
      require(off >= 0 && off <= file.length, s"Invalid measurement address $address for segment ${segment.getName}.")
      off
    }

    override def close(): Unit = file.close()
  }

  case class SegmentIndex(segments: Seq[FileBackedSegment]) extends AutoCloseable {

    // Use the 'start' as the key to enable binary search behavior.
    private val tree: TreeMap[Long, FileBackedSegment] = TreeMap(
      segments.map(s => s.segment.getAddress -> s): _*
    )

    def apply(m: Measurement): FileBackedSegment = get(m).get

    def get(m: Measurement): Option[FileBackedSegment] =
      get(m.getEcuAddress)

    def get(i: Long): Option[FileBackedSegment] = {
      tree.rangeTo(i).lastOption.map(_._2).filter(s => i <= s.segment.getAddress + s.segment.getSize)
    }

    override def close(): Unit = tree.values.foreach(_.close())
  }

  val isVariableRAM: MemorySegment => Boolean = segment =>
    segment.getPrgType == PrgType.VARIABLES && segment.getMemoryType == MemoryType.RAM

  def buildFileBackedSegment(segment: MemorySegment): FileBackedSegment = {
    val tmpFile = Files.createTempFile("a2lBinAdapter", segment.getName)
    tmpFile.toFile.deleteOnExit()
    val sz       = segment.getSize
    val initData = Array.fill[Byte](sz.toInt)(0.toByte)
    Files.write(tmpFile, initData)
    FileBackedSegment(segment, new RandomAccessFile(tmpFile.toFile, "rw"))
  }

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
