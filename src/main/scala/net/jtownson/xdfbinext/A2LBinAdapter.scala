package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.{ConversionType, DataType}
import net.alenzen.a2l.{AxisPts, Characteristic, CompuMethod}
import net.jtownson.xdfbinext.RatFunFormula.RatFun

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.{DOWN, HALF_UP}

/** What would be nice here would be to convert an a2l+bin to some kind of repr where we are able to answer questions
  * such as 'find axes where the units are kg/h and the values are below 1300 kg/h.
  */
class A2LBinAdapter(val bin: File, a2l: A2LWrapper, offset: Long = 0x9000000) {

  def readValue(cName: String): BigDecimal = {
    readCharacteristic(cName).head
  }

  def readCurve(cName: String): Interpolated1D = {
    val values = readCharacteristic(cName)
    val axis   = readAxis(a2l.getXAxis(a2l.characteristics(cName)).getName)
    Interpolated1D(axis, values)
  }

  def readMap(cName: String): Interpolated2D = {
    val values = readCharacteristic(cName)
    val xAxis  = readAxis(a2l.getXAxis(a2l.characteristics(cName)).getName)
    val yAxis  = readAxis(a2l.getYAxis(a2l.characteristics(cName)).getName)
    Interpolated2D(xAxis, yAxis, values)
  }

  def readAxis(aName: String): Array[BigDecimal] = {
    val a         = a2l.axisPts(aName)
    val formula   = getFormula(a)
    val (len, dp) = a2l.getFormat(a)
    val cellCount = a2l.getCellCount(a)

    a2l.getType(a) match
      case DataType.SBYTE =>
        ???
      case DataType.UBYTE =>
        ???
      case DataType.SWORD =>
        readSWord(offsetAddress(a), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.UWORD =>
        readUWord(offsetAddress(a), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.SLONG =>
        ???
      case DataType.ULONG =>
        ???
      case _ =>
        ???

  }

  def readCharacteristic(cName: String): Array[BigDecimal] = {
    readCharacteristic(a2l.characteristics(cName))
  }

  private def readCharacteristic(c: Characteristic): Array[BigDecimal] = {
    val formula   = getFormula(c)
    val (len, dp) = a2l.getFormat(c)
    val cellCount = a2l.getCellCount(c)

    a2l.getType(c) match
      case DataType.SBYTE =>
        readSByte(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.UBYTE =>
        readUByte(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.SWORD =>
        readSWord(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.UWORD =>
        readUWord(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.SLONG =>
        readSLong(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case DataType.ULONG =>
        readULong(offsetAddress(c), cellCount)
          .map(BigDecimal(_))
          .map(formula.apply)
          .map(_.setScale(dp, HALF_UP))
      case _ =>
        ???
  }

  private def getFormula(c: Characteristic): RatFun = {
    val compuMethod = a2l.compuMethods(c.getConversion)
    getFormula(compuMethod)
  }

  private def getFormula(a: AxisPts): RatFun = {
    val compuMethod = a2l.compuMethods(a.getConversion)
    getFormula(compuMethod)
  }

  private def getFormula(compuMethod: CompuMethod): RatFun = {
    if (compuMethod.getConversionType == ConversionType.RAT_FUNC) {
      val coeffs = compuMethod.getCoeffs
      RatFun(coeffs.getA, coeffs.getB, coeffs.getC, coeffs.getD, coeffs.getE, coeffs.getF)
    } else {
      RatFun(0, 1, 0, 0, 0, 1) // TODO consider doing this properly
    }
  }

  private def offsetAddress(c: Characteristic): Long =
    c.getAddress - offset

  private def offsetAddress(a: AxisPts): Long =
    a.getAddress - offset

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

  private def readSByte(offset: Long, len: Int): Array[Byte] = {
    val a = new Array[Byte](len)
    binAccess.seek(offset)
    binAccess.read(a)
    a
  }

  private def readUByte(offset: Long, len: Int): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(offset)
    (0 until len).foreach(i => a.addOne(binAccess.readUnsignedByte()))
    a.toArray
  }

  private def readSWord(offset: Long, len: Int): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(offset)
    (0 until len).foreach(i => a.addOne(binAccess.readShort()))
    a.toArray
  }

  private def readUWord(offset: Long, len: Int): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(offset)
    (0 until len).foreach(i => a.addOne(binAccess.readUnsignedShort()))
    a.toArray
  }

  private def readSLong(offset: Long, len: Int): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(offset)
    (0 until len).foreach(i => a.addOne(binAccess.readInt()))
    a.toArray
  }

  private def readULong(offset: Long, len: Int): Array[Long] = {
    val a = new Array[Byte](len * 4)
    binAccess.seek(offset)
    binAccess.read(a)
    val wrapped = ByteBuffer.wrap(a).asIntBuffer()
    val toBuff  = new ArrayBuffer[Long](len)
    while (wrapped.hasRemaining) {
      val intVal  = wrapped.get
      val longVal = intVal & 0xffffffffL
      toBuff.addOne(longVal)
    }
    toBuff.toArray
  }
}
