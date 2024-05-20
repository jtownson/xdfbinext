package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.enums.DataType.*
import net.jtownson.xdfbinext.a2l.RatFunFormula.RatFun

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode.HALF_UP

trait ValueConsumer {
  def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyVTab(vtab: CompuVTab): Array[String]

  def applyTab(tab: CompuTab): Array[BigDecimal]
}

object ValueConsumer {

  def apply(dataType: DataType, address: Long, len: Int, binAccess: RandomAccessFile): ValueConsumer =
    dataType match {
      case SBYTE =>
        SByteValueConsumer(address, len, binAccess)
      case UBYTE =>
        UByteValueConsumer(address, len, binAccess)
      case SWORD =>
        SWordValueConsumer(address, len, binAccess)
      case UWORD =>
        UWordValueConsumer(address, len, binAccess)
      case SLONG =>
        SLongValueConsumer(address, len, binAccess)
      case ULONG =>
        ULongValueConsumer(address, len, binAccess)
      case FLOAT32_IEEE =>
        Float32ValueConsumer(address, len, binAccess)
      case _ =>
        ???
    }

  private def readSByte(address: Long, len: Int, binAccess: RandomAccessFile): Array[Byte] = {
    val a = new Array[Byte](len)
    binAccess.seek(address)
    binAccess.read(a)
    a
  }

  private def readUByte(address: Long, len: Int, binAccess: RandomAccessFile): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(address)
    (0 until len).foreach(i => a.addOne(binAccess.readUnsignedByte()))
    a.toArray
  }

  private def readSWord(address: Long, len: Int, binAccess: RandomAccessFile): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(address)
    (0 until len).foreach(i => a.addOne(binAccess.readShort()))
    a.toArray
  }

  private def readUWord(address: Long, len: Int, binAccess: RandomAccessFile): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(address)
    (0 until len).foreach(i => a.addOne(binAccess.readUnsignedShort()))
    a.toArray
  }

  private def readSLong(address: Long, len: Int, binAccess: RandomAccessFile): Array[Int] = {
    val a = new ArrayBuffer[Int](len)
    binAccess.seek(address)
    (0 until len).foreach(i => a.addOne(binAccess.readInt()))
    a.toArray
  }

  private def readULong(address: Long, len: Int, binAccess: RandomAccessFile): Array[Long] = {
    val a = new Array[Byte](len * 4)
    binAccess.seek(address)
    binAccess.read(a)
    val wrapped = ByteBuffer.wrap(a).asIntBuffer()
    val toBuff = new ArrayBuffer[Long](len)
    while (wrapped.hasRemaining) {
      val intVal = wrapped.get
      val longVal = intVal & 0xffffffffL
      toBuff.addOne(longVal)
    }
    toBuff.toArray
  }

  private def readFloat32(address: Long, len: Int, binAccess: RandomAccessFile): Array[Float] = {
    binAccess.seek(address)
    (0 until len).map(_ => binAccess.readFloat()).toArray
  }

  private def readFloat64(address: Long, len: Int, binAccess: RandomAccessFile): Array[Double] = {
    binAccess.seek(address)
    (0 until len).map(_ => binAccess.readDouble()).toArray
  }

  case class SByteValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSByte(address, len, binAccess).map(b => vtab.tab.getOrElse(b.toInt, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSByte(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readSByte(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
  }

  case class UByteValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readUByte(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readUByte(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readUByte(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
  }

  case class SWordValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSWord(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSWord(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readSWord(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class UWordValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readUWord(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readUWord(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readUWord(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class SLongValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSLong(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSLong(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readSLong(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class ULongValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readULong(address, len, binAccess).map(b => vtab.tab.getOrElse(b.toInt, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readULong(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readULong(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class Float32ValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readFloat32(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readFloat32(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyVTab(vtab: CompuVTab): Array[String] =
      throw new UnsupportedOperationException("Cannot perform vtab lookup for float32 storage type.")
  }

  case class Float64ValueConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends ValueConsumer {

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readFloat64(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readFloat64(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyVTab(vtab: CompuVTab): Array[String] =
      throw new UnsupportedOperationException("Cannot perform vtab lookup for float32 storage type.")
  }
}
