package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.BinAdapter.{data2Str1D, data2Str2D, data2StrConst}
import net.jtownson.xdfbinext.XdfSchema.{Table1DEnriched, Table2DEnriched, XdfModel, XdfTable}

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

class BinAdapter(val bin: File, xdfModel: XdfModel) {

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

  def tableRead(tableName: String): Array[BigDecimal] = {
    tableRead(xdfModel.tablesByName(tableName))
  }

  private def tableRead(xdfTable: XdfTable): Array[BigDecimal] = {
    applyDecimalPl(xdfTable)(tableDyn(xdfTable))
  }

  private def tableReadOrX(table: XdfTable, maybeX: Option[XdfTable]): Array[String] = {
    maybeX.fold(table.xLabels.toArray)(t => tableRead(t).map(_.toString))
  }

  private def tableReadOrY(table: XdfTable, maybeY: Option[XdfTable]): Array[String] = {
    maybeY.fold(table.yLabels.toArray)(t => tableRead(t).map(_.toString))
  }

  private def applyDecimalPl(t: XdfTable)(in: Array[BigDecimal]): Array[BigDecimal] = {
    in.map(bd => bd.setScale(t.axes.z.decimalPl, RoundingMode.HALF_UP))
  }

  private def readRaw(table: XdfTable): Array[Byte] = {
    val startAddress  = table.axes.z.embeddedData.mmedAddress
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount
    val a             = new Array[Byte](cellSizeBytes * numCells)
    binAccess.seek(startAddress)
    binAccess.read(a)
    a
  }

  private def readUnsignedByte(table: XdfTable): Array[Int] = {
    val startAddress  = table.axes.z.embeddedData.mmedAddress
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount
    val a             = new ArrayBuffer[Int](cellSizeBytes * numCells)

    binAccess.seek(startAddress)
    (0 until numCells).foreach(i => a.addOne(binAccess.readUnsignedByte()))
    a.toArray
  }

  private def tableByte(table: XdfTable): Array[BigDecimal] = {
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val isSigned      = table.axes.z.embeddedData.mmedTypeFlags == XdfSchema.signedFlag
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount
    require(cellSizeBytes == 1, s"Invalid cell size for tableByte: ${table.title} has size in bytes $cellSizeBytes")

    if (isSigned) {
      val t                            = readRaw(table)
      val equation: Byte => BigDecimal = EquationParser.parseByteF1(table.axes.z.math.equation)
      t.map(equation)
    } else {
      val t                           = readUnsignedByte(table)
      val equation: Int => BigDecimal = EquationParser.parseIntF1(table.axes.z.math.equation)
      t.map(equation)
    }

  }

  private def tableShort(table: XdfTable): Array[BigDecimal] = {
    val tableRaw = readRaw(table)

    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val isSigned      = table.axes.z.embeddedData.mmedTypeFlags == XdfSchema.signedFlag
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount

    require(cellSizeBytes == 2, s"Invalid cell size for tableShort: ${table.title} has size in bytes $cellSizeBytes")

    val wrapped: ByteBuffer = ByteBuffer.wrap(tableRaw)
    val shortBuff           = wrapped.asShortBuffer()
    val toBuff              = new ArrayBuffer[Int](numCells)

    while (shortBuff.hasRemaining) {
      val shortVal    = shortBuff.get
      val intVal: Int = if (isSigned || shortVal >= 0) shortVal.toInt else 0x10000 + shortVal
      toBuff.addOne(intVal)
    }

    val equation: Int => BigDecimal = EquationParser.parseIntF1(table.axes.z.math.equation)

    toBuff.map(equation).toArray
  }

  private def tableInt(table: XdfTable): Array[BigDecimal] = {
    val tableRaw = readRaw(table)

    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount

    val wrapped: ByteBuffer = ByteBuffer.wrap(tableRaw)

    require(cellSizeBytes == 4, s"Invalid cell size for tableShort: ${table.title} has size in bytes $cellSizeBytes")

    val isFloatingPoint = table.axes.z.embeddedData.mmedTypeFlags == XdfSchema.floatingPointFlag

    if (isFloatingPoint) {
      val floatBuff = wrapped.asFloatBuffer()
      val toBuff    = new ArrayBuffer[BigDecimal](numCells)
      while (floatBuff.hasRemaining) {
        val f = floatBuff.get()
        toBuff.addOne(BigDecimal(f))
      }
      val equation: BigDecimal => BigDecimal = EquationParser.parseBigDecimalF1(table.axes.z.math.equation)

      toBuff.map(equation).toArray

    } else {
      val intBuff = wrapped.asIntBuffer()
      val toBuff  = new ArrayBuffer[Long](numCells)
      while (intBuff.hasRemaining) {
        val intVal  = intBuff.get
        val longVal = intVal & 0xffffffffL
        toBuff.addOne(longVal)
      }

      val equation: Long => BigDecimal = EquationParser.parseLongF1(table.axes.z.math.equation)

      toBuff.map(equation).toArray
    }
  }

  def tableDyn(table: XdfTable): Array[BigDecimal] = {
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount

    if (cellSizeBytes == 1)
      tableByte(table)
    else if (cellSizeBytes == 2)
      tableShort(table)
    else if (cellSizeBytes == 4)
      tableInt(table)
    else
      throw new UnsupportedOperationException(s"No handling of cells with size $cellSizeBytes bytes")
  }

  def tableReadStr(name: String): String = {
    xdfModel.table(name) match {
      case t: XdfTable =>
        dataToStrConst(t)
      case t: Table1DEnriched =>
        dataToStr1D(t)
      case t: Table2DEnriched =>
        dataToStr2D(t)
    }
  }

  def dataToStrConst(table: XdfTable): String = {
    data2StrConst(tableRead(table).head)
  }

  def dataToStr1D(enrichedTable: Table1DEnriched): String = {
    val tableData = tableRead(enrichedTable.table)
    val xAxisData = tableReadOrX(enrichedTable.table, enrichedTable.xAxisBreakpoints)
    data2Str1D(xAxisData, tableData)
  }

  def dataToStr2D(enrichedTable: Table2DEnriched): String = {
    val xAxisData = tableReadOrX(enrichedTable.table, enrichedTable.xAxisBreakpoints)
    val yAxisData = tableReadOrY(enrichedTable.table, enrichedTable.yAxisBreakpoints)
    val tableData = tableRead(enrichedTable.table)
    data2Str2D(xAxisData, yAxisData, tableData)
  }

}

object BinAdapter {

  def data2StrConst(tableData: BigDecimal): String = {
    new StringBuilder().append(f"$tableData%9s").append("\n").toString
  }

  def data2Str1D(xAxisData: Array[String], tableData: Array[BigDecimal]): String = {
    val cols        = xAxisData.length
    val xAxisHeader = (0 until cols).map { col => f"${xAxisData(col)}%9s" }.mkString
    val rowStr      = (0 until cols).map(col => f"${tableData(col)}%9s").mkString
    new StringBuilder().append(xAxisHeader).append("\n").append(rowStr).append("\n").toString
  }

  def data2Str2D(
      xAxisData: Array[String],
      yAxisData: Array[String],
      tableData: Array[BigDecimal]
  ): String = {
    val cols        = xAxisData.length
    val rows        = yAxisData.length
    val sb          = new StringBuilder()
    val xAxisHeader = (0 until cols).map { col => f"${xAxisData(col)}%9s" }.mkString
    sb.append(" ".repeat(9)).append(xAxisHeader).append("\n")
    (0 until rows).map { row =>
      sb.append(f"${yAxisData(row)}%9s")
      val rowStr = (0 until cols).map { col =>
        val i = row * cols + col
        f"${tableData(i)}%9s"
      }.mkString
      sb.append(rowStr).append("\n")
    }
    sb.toString
  }

  case class BinAdapterCompare(lhs: String, diff: String, rhs: String) {}

  def compare(xdfModel: XdfModel, lhs: File, rhs: File): Map[String, BinAdapterCompare] = {
    xdfModel.tablesByName.keySet
      .map(tableName => tableName -> compare(tableName, xdfModel, lhs, rhs))
      .toMap
      .collect { case (name, Some(comparison)) => name -> comparison }
  }

  def compare(tableName: String, xdfModel: XdfModel, lhs: File, rhs: File): Option[BinAdapterCompare] = {
    val table = xdfModel.tablesByName(tableName)

    val lhsb = new BinAdapter(lhs, xdfModel)
    val rhsb = new BinAdapter(rhs, xdfModel)

    val tl = lhsb.tableReadStr(tableName)
    val tr = rhsb.tableReadStr(tableName)

    val dl = lhsb.tableDyn(table)
    val dr = rhsb.tableDyn(table)

    val diff = rhsb.applyDecimalPl(table)(dl.zip(dr).map { case (lbd, rbd) => rbd - lbd })

    xdfModel.table(tableName) match
      case t: XdfTable =>
        if (dl.sameElements(dr)) {
          None
        } else {
          Some(BinAdapterCompare(tl, data2StrConst(diff.head), tr))
        }
      case t: Table1DEnriched =>
        val xLh = t.xAxisBreakpoints.map(x => lhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        val xRh = t.xAxisBreakpoints.map(x => rhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        if (dl.sameElements(dr) && xLh.sameElements(xRh)) {
          None
        } else {
          val td = data2Str1D(rhsb.tableReadOrX(t.table, t.xAxisBreakpoints), diff)
          Some(BinAdapterCompare(tl, td, tr))
        }
      case t: Table2DEnriched =>
        val xLh = t.xAxisBreakpoints.map(x => lhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        val xRh = t.xAxisBreakpoints.map(x => rhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])

        val yLh = t.yAxisBreakpoints.map(y => lhsb.tableDyn(y)).getOrElse(Array.empty[BigDecimal])
        val yRh = t.yAxisBreakpoints.map(y => rhsb.tableDyn(y)).getOrElse(Array.empty[BigDecimal])
        if (dl.sameElements(dr) && xLh.sameElements(xRh) && yLh.sameElements(yRh)) {
          None
        } else {
          val td = data2Str2D(
            rhsb.tableReadOrX(t.table, t.xAxisBreakpoints),
            rhsb.tableReadOrY(t.table, t.yAxisBreakpoints),
            diff
          )
          Some(BinAdapterCompare(tl, td, tr))
        }
  }
}
