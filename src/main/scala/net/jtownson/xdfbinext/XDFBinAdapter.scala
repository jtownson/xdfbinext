package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.Data2Str.{data2Str1D, data2Str2D, data2StrConst}
import net.jtownson.xdfbinext.XDFBinAdapter.*
import net.jtownson.xdfbinext.XdfSchema.*

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.HALF_UP

class XDFBinAdapter(val bin: File, val xdfModel: XdfModel) {

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

  private val constants: Map[String, Array[BigDecimal]] =
    xdfModel.tablesConstant.map((tableName, _) => tableName -> tableRead(tableName))

  private val tables1D: Map[String, Array[BigDecimal]] =
    xdfModel.tables1D.map((tableName, _) => tableName -> tableRead(tableName))

  private val tables2D: Map[String, Array[BigDecimal]] =
    xdfModel.tables2D.map((tableName, _) => tableName -> tableRead(tableName))

  private val virtualTables: Map[String, Interpolated2D] =
    xdfModel.virtualTablesByName.map((tableName, _) => tableName -> virtualTableRead(tableName))

  private def virtualTableRead(tableName: String): Interpolated2D = {
    xdfModel.virtualTablesByName(tableName).tableDef match
      case InverseLookup2D(baseTableName, invert) =>
        inverseLookup(baseTableName, invert)
      case XdfExpression(aliases, expression) =>
        xdfExpression(aliases, expression)
  }

  private def xdfExpression(aliases: Map[String, String], expression: String): Interpolated2D = {
    val aliasesBin = aliases.map((label, tableName) => (label, tableRead2D(tableName).data))
    XdfExpressionParser.from(aliasesBin).parseTableExpr(expression)
  }

  private def inverseLookup(baseTableName: String, invert: String) = {
    val xdfBase = xdfModel.tables2D(baseTableName)
    val tBase   = tableRead2D(baseTableName)
    if (invert == InverseLookup2D.x)
      val (xp, yp, zp) = Invert.tableInvertX(tBase.data.xAxis, tBase.data.yAxis, tBase.data.values)
      Interpolated2D(xp, yp, zp)
    else
      require(invert == InverseLookup2D.y, s"Invalid invert axis '$invert'.")
      ???
  }

  def virtualTableByName(tableName: String): Interpolated2D = {
    virtualTables(tableName)
  }

  /** Read table data into a flat array.
    *
    * @param tableName
    * @return
    */
  def tableRead(tableName: String): Array[BigDecimal] = {
    tableRead(xdfModel.tablesByName(tableName))
  }

  def tableReadConst(tableName: String): BinConst = {
    val xdfTable   = xdfModel.tablesConstant(tableName)
    val tableValue = tableRead(tableName).head
    BinConst(xdfTable, tableValue)
  }

  def tableRead1D(tableName: String): BinTable1D = {
    val xdfTable    = xdfModel.tables1D(tableName)
    val xAxisDef    = xdfTable.xAxisBreakpoints.getOrElse(throw new UnsupportedOperationException())
    val xAxis       = tableRead(xAxisDef.title)
    val tableValues = tables1D(tableName)

    require(
      isMonotonicallyIncreasing(xAxis),
      s"non-monotonic breakpoint axis for table $tableName, axis name ${xAxisDef.title}."
    )
    require(
      tableValues.length == xAxis.length,
      s"Mismatched breakpoints for table $tableName, axis name ${xAxisDef.title}."
    )

    BinTable1D(xdfTable, Interpolated1D(xAxis, tableValues))
  }

  def tableRead2D(tableName: String): BinTable2D = {
    val xdfTable = xdfModel.tables2D(tableName)
    val xAxisDef = xdfTable.xAxisBreakpoints.getOrElse(throw new UnsupportedOperationException())
    val yAxisDef = xdfTable.yAxisBreakpoints.getOrElse(throw new UnsupportedOperationException())

    val xAxis = tableRead(xAxisDef.title)
    val yAxis = tableRead(yAxisDef.title)

    val tableValues = tables2D(tableName)

    require(
      isMonotonicallyIncreasing(xAxis),
      s"non-monotonic breakpoint x-axis for table $tableName, axis name ${xAxisDef.title}."
    )
    require(
      isMonotonicallyIncreasing(yAxis),
      s"non-monotonic breakpoint y-axis for table $tableName, axis name ${yAxisDef.title}."
    )
    require(
      tableValues.length == xAxis.length * yAxis.length,
      s"Mismatched breakpoints for table $tableName. Axes ${xAxisDef.title} and ${yAxisDef.title}."
    )

    BinTable2D(xdfTable, Interpolated2D(xAxis, yAxis, tableValues))
  }

  /** Read table data as a printable string.
    * @param name
    * @return
    */
  def tableReadStr(name: String): String = {
    xdfModel.table(name) match {
      case t: XdfTable =>
        dataToStrConst(t)
      case t: XdfTable1D =>
        dataToStr1D(t)
      case t: XdfTable2D =>
        dataToStr2D(t)
    }
  }

  private def isMonotonicallyIncreasing(decimals: Array[BigDecimal]): Boolean = decimals.sorted.sameElements(decimals)

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

  private def tableRead(xdfTable: XdfTable): Array[BigDecimal] = {
    applyDecimalPl(xdfTable)(tableDyn(xdfTable))
  }

  def tableReadOrX(table: XdfTable, maybeX: Option[XdfTable]): Array[String] = {
    maybeX.fold(table.xLabels.toArray)(t => tableRead(t).map(_.toString))
  }

  def tableReadOrY(table: XdfTable, maybeY: Option[XdfTable]): Array[String] = {
    maybeY.fold(table.yLabels.toArray)(t => tableRead(t).map(_.toString))
  }

  def applyDecimalPl(t: XdfTable)(in: Array[BigDecimal]): Array[BigDecimal] = {
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

  private def dataToStrConst(table: XdfTable): String = {
    data2StrConst(tableRead(table).head)
  }

  private def dataToStr1D(enrichedTable: XdfTable1D): String = {
    val tableData = tableRead(enrichedTable.table)
    val xAxisData = tableReadOrX(enrichedTable.table, enrichedTable.xAxisBreakpoints)
    data2Str1D(xAxisData, tableData)
  }

  private def dataToStr2D(enrichedTable: XdfTable2D): String = {
    val xAxisData = tableReadOrX(enrichedTable.table, enrichedTable.xAxisBreakpoints)
    val yAxisData = tableReadOrY(enrichedTable.table, enrichedTable.yAxisBreakpoints)
    val tableData = tableRead(enrichedTable.table)
    data2Str2D(xAxisData, yAxisData, tableData)
  }
}

object XDFBinAdapter {

  case class BinConst(xdfTable: XdfTable, value: BigDecimal)

  case class BinTable1D(xdfTable: XdfTable1D, data: Interpolated1D)

  // TODO consider real and virtual tables under some common abstraction
  // really all we care about here is the data, the axes, the units and perhaps some notion of the base data type or precision.
  // could extract that info to a new type. Map XdfTable2D to that type and do likewise for virtual tables somehow.
  case class BinTable2D(xdfTable: XdfTable2D, data: Interpolated2D)

}
