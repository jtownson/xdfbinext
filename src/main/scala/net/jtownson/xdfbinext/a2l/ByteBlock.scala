package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.{DataSize, DataType}
import net.alenzen.a2l.enums.DataType.*
import net.alenzen.a2l.{AxisPtsXYZ45, FncValues, NoAxisPtsXYZ45, RecordLayout, Reserved}
import scala.jdk.CollectionConverters._

object ByteBlock {

  private case class ByteBlockRel(label: String, position: Int, dataType: DataType, cellCount: Int) {
    val cellSize: Int  = sizeOf(dataType)
    val sizeBytes: Int = cellSize * cellCount
  }

  case class ByteBlockAbs(label: String, address: Long, dataType: DataType, cellCount: Int)

  private def getDataTypeFromSz(dataSize: DataSize): DataType = dataSize match
    case DataSize.BYTE =>
      UBYTE
    case DataSize.WORD =>
      UWORD
    case DataSize.LONG =>
      ULONG

  private def reservedByteBlocksRel(reserved: List[Reserved]): Seq[ByteBlockRel] =
    reserved.map(r => ByteBlockRel(unusedLabel, r.getPosition.toInt, getDataTypeFromSz(r.getDataSize), 1))

  private def toByteBlockRel(nax: NoAxisPtsXYZ45): ByteBlockRel =
    ByteBlockRel(unusedLabel, nax.getPosition.toInt, nax.getDataType, 1)

  private def toByteBlockRel(label: String, nAxisPoints: Int)(ax: AxisPtsXYZ45): ByteBlockRel =
    ByteBlockRel(label, ax.getPosition.toInt, ax.getDatatype, nAxisPoints)

  private def toByteBlockRel(nFnPoints: Int)(fn: FncValues): ByteBlockRel =
    ByteBlockRel(fnLabel, fn.getPosition.toInt, fn.getDataType, nFnPoints)

  private def toByteBlocksRel(nAxisPointsX: Int, nAxisPointsY: Int, rl: RecordLayout): Seq[ByteBlockRel] = {
    reservedByteBlocksRel(rl.getReserved.asScala.toList) ++
      Seq(
        Option(rl.getNoAxisPtsX).map(toByteBlockRel),
        Option(rl.getNoAxisPtsY).map(toByteBlockRel),
        Option(rl.getAxisPtsX).map(toByteBlockRel(xLabel, nAxisPointsX)),
        Option(rl.getAxisPtsY).map(toByteBlockRel(yLabel, nAxisPointsY)),
        Option(rl.getFunctionValues).map(toByteBlockRel(nAxisPointsX * nAxisPointsY))
      ).flatten
  }

  def toRecord(baseAddress: Long, nAxisPointsX: Int, nAxisPointsY: Int, rl: RecordLayout): Map[String, ByteBlockAbs] =
    toRecord(toByteBlocksAbs(baseAddress, toByteBlocksRel(nAxisPointsX, nAxisPointsY, rl)))

  private def toByteBlocksAbs(baseAddress: Long, bbs: Seq[ByteBlockRel]): Seq[ByteBlockAbs] = {
    val z0 = (baseAddress, Vector.empty[ByteBlockAbs])

    bbs
      .sortBy(_.position)
      .foldLeft(z0) { (acc, nextBB) =>
        val (address, absBbs) = acc
        val bAbs              = ByteBlockAbs(nextBB.label, address, nextBB.dataType, nextBB.cellCount)
        (address + nextBB.sizeBytes, absBbs :+ bAbs)
      }
      ._2
  }

  def toRecord(bbs: Seq[ByteBlockAbs]): Map[String, ByteBlockAbs] =
    bbs.filterNot(_.label == unusedLabel).map(bb => bb.label -> bb).toMap

  val xLabel      = "x"
  val yLabel      = "y"
  val fnLabel     = "fn"
  val unusedLabel = "-"

  private val sizeOf: Map[DataType, Int] = Map(
    SBYTE        -> 1,
    UBYTE        -> 1,
    SWORD        -> 2,
    UWORD        -> 2,
    SLONG        -> 4,
    ULONG        -> 4,
    FLOAT32_IEEE -> 4,
    FLOAT64_IEEE -> 8
  )
}
