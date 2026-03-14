package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.XdfTable

import java.io.File
import scala.collection.mutable

class XdfTableRelocator(sourceAdapter: XDFBinAdapter) {

  import XdfTableRelocator.*

  def relocateAll(tableName: String, targetBin: File, topN: Int = 10): Seq[RelocationCandidate] = {

    val xdfModel    = sourceAdapter.xdfModel
    val sourceTable = xdfModel.tablesByName(tableName)

    xdfModel.tables1D
      .get(tableName)
      .map { t1d =>
        relocate(tableName, targetBin, topN) ++
          t1d.xAxisBreakpoints.toSeq.flatMap(xbp => relocate(xbp.title, targetBin, topN))
      }
      .orElse {
        xdfModel.tables2D.get(tableName).map { t2d =>
          relocate(tableName, targetBin, topN) ++
            t2d.xAxisBreakpoints.toSeq.flatMap(xbp => relocate(xbp.title, targetBin, topN)) ++
            t2d.yAxisBreakpoints.toSeq.flatMap(ybp => relocate(ybp.title, targetBin, topN))
        }
      }
      .getOrElse(Seq.empty)
  }

  def relocate(
      tableName: String,
      targetBin: File,
      topN: Int = 10
  ): Seq[RelocationCandidate] = {

    val sourceTable = sourceAdapter.xdfModel.tablesByName(tableName)

    val sourceData: Array[Double] = sourceAdapter.tableRead(tableName).map(_.toDouble)
    val windowSizeBytes           = tableSizeBytes(sourceTable)

    val fileLength         = targetBin.length()
    val sourceTableAddress = sourceTable.axes.z.embeddedData.mmedAddress
    val from               = math.max(0L, sourceTableAddress - 8120 * 2)
    val until              = math.min(sourceTableAddress + 8120 * 2, fileLength)

    val targetAdapter = new XDFBinAdapter(targetBin, sourceAdapter.xdfModel)

    (from to until)
      .foldLeft(Vector.empty[RelocationCandidate]) { (acc, nextAddr) =>
        val candidateTable = tableAtAddress(sourceTable, nextAddr)
        val candidateValues =
          XDFBinAdapter.applyDecimalPl(candidateTable)(targetAdapter.tableDyn(candidateTable)).map(_.toDouble)
        val variance = sumSquaredDifference(sourceData, candidateValues)
        acc :+ RelocationCandidate(tableName, nextAddr, variance)
      }
      .sortBy(_.variance)
      .take(topN)
  }

  private def pushBest(
      heap: mutable.PriorityQueue[RelocationCandidate],
      candidate: RelocationCandidate,
      topN: Int
  ): Unit = {
    if (heap.size < topN)
      heap.enqueue(candidate)
    else if (candidate.variance < heap.head.variance) {
      heap.dequeue()
      heap.enqueue(candidate)
    }
  }

  private def tableAtAddress(table: XdfTable, newAddress: Long): XdfTable = {
    table.copy(
      axes = table.axes.copy(
        z = table.axes.z.copy(
          embeddedData = table.axes.z.embeddedData.copy(mmedAddress = newAddress)
        )
      )
    )
  }

  private def tableSizeBytes(table: XdfTable): Long = {
    val cellCount     = table.axes.x.indexCount * table.axes.y.indexCount
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8L
    cellCount * cellSizeBytes
  }

  private def sumSquaredDifference(a: Array[Double], b: Array[Double]): Double = {
    require(a.length == b.length, s"Mismatched array lengths: ${a.length} vs ${b.length}")
    var i   = 0
    var ssd = 0.0
    while (i < a.length) {
      val d = a(i) - b(i)
      ssd += d * d
      i += 1
    }
    ssd
  }
}

object XdfTableRelocator {
  case class RelocationCandidate(table: String, address: Long, variance: Double) {
    override def toString: String = s"0x${address.toHexString.toUpperCase}, $variance"
  }
}
