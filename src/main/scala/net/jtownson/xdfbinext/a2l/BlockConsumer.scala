package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.RecordLayout
import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.enums.DataType.*
import net.jtownson.xdfbinext.a2l.ByteBlock.{ByteBlockAbs, toRecord}

import java.io.RandomAccessFile
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode.HALF_UP

trait BlockConsumer {
  def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyVTab(vtab: CompuVTab): Array[String]

  def applyTab(tab: CompuTab): Array[BigDecimal]
}

object BlockConsumer {
  
  def toTypedConsumableRecord(
      baseAddress: Long,
      nAxisPointsX: Int,
      nAxisPointsY: Int,
      rl: RecordLayout,
      binAccess: RandomAccessFile
  ): Map[String, BlockConsumer] =
    toTypedConsumableRecord(toRecord(baseAddress, nAxisPointsX, nAxisPointsY, rl), binAccess)

  private def toTypedConsumableRecord(
      blockRecord: Map[String, ByteBlockAbs],
      binAccess: RandomAccessFile
  ): Map[String, BlockConsumer] =
    blockRecord.map((name, block) => (name, BlockConsumer(block, binAccess)))

  private def apply(bb: ByteBlockAbs, binAccess: RandomAccessFile): BlockConsumer =
    BlockConsumer(bb.dataType, bb.address, bb.cellCount, binAccess)

  private def apply(dataType: DataType, address: Long, cellCount: Int, binAccess: RandomAccessFile): BlockConsumer =
    dataType match {
      case SBYTE =>
        SByteBlockConsumer(address, cellCount, binAccess)
      case UBYTE =>
        UByteBlockConsumer(address, cellCount, binAccess)
      case SWORD =>
        SWordBlockConsumer(address, cellCount, binAccess)
      case UWORD =>
        UWordBlockConsumer(address, cellCount, binAccess)
      case SLONG =>
        SLongBlockConsumer(address, cellCount, binAccess)
      case ULONG =>
        ULongBlockConsumer(address, cellCount, binAccess)
      case FLOAT32_IEEE =>
        Float32BlockConsumer(address, cellCount, binAccess)
      case FLOAT64_IEEE =>
        Float64BlockConsumer(address, cellCount, binAccess)
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
    val toBuff  = new ArrayBuffer[Long](len)
    while (wrapped.hasRemaining) {
      val intVal  = wrapped.get
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

  case class SByteBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSByte(address, len, binAccess).map(b => vtab.tab.getOrElse(b.toInt, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSByte(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readSByte(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
  }

  case class UByteBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readUByte(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readUByte(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readUByte(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
  }

  case class SWordBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSWord(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSWord(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readSWord(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class UWordBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readUWord(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readUWord(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readUWord(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class SLongBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readSLong(address, len, binAccess).map(b => vtab.tab.getOrElse(b, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readSLong(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readSLong(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class ULongBlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {
    def applyVTab(vtab: CompuVTab): Array[String] =
      readULong(address, len, binAccess).map(b => vtab.tab.getOrElse(b.toInt, b.toString))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readULong(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
      readULong(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))
    }
  }

  case class Float32BlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readFloat32(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readFloat32(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyVTab(vtab: CompuVTab): Array[String] =
      throw new UnsupportedOperationException("Cannot perform vtab lookup for float32 storage type.")
  }

  case class Float64BlockConsumer(address: Long, len: Int, binAccess: RandomAccessFile) extends BlockConsumer {

    override def applyFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
      readFloat64(address, len, binAccess).map(BigDecimal(_)).map(ratFun.apply).map(_.setScale(dp, HALF_UP))

    def applyTab(tab: CompuTab): Array[BigDecimal] =
      readFloat64(address, len, binAccess).map(BigDecimal(_)).map(b => tab.interpolated.atX(b))

    override def applyVTab(vtab: CompuVTab): Array[String] =
      throw new UnsupportedOperationException("Cannot perform vtab lookup for float32 storage type.")
  }
}
