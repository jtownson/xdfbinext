package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.{XdfModel, XdfTable}

import java.io.{File, RandomAccessFile}
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

class BinAdapter(val bin: File, xdfModel: XdfModel) {

  private val binAccess: RandomAccessFile = new RandomAccessFile(bin, "r")

  private def readRaw(table: XdfTable): Array[Byte] = {
    val startAddress  = table.axes.z.embeddedData.mmedAddress
    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount
    val a             = new Array[Byte](cellSizeBytes * numCells)
    binAccess.seek(startAddress)
    binAccess.read(a)
    a
  }

  def tableShort(name: String): Array[Short] = {
    val table    = xdfModel.tablesByName(name)
    val tableRaw = readRaw(table)

    val cellSizeBytes = table.axes.z.embeddedData.mmedElementSizeBits / 8
    val numCells      = table.axes.x.indexCount * table.axes.y.indexCount

    val wrapped: ByteBuffer = ByteBuffer.wrap(tableRaw)

    require(cellSizeBytes == 2, s"Invalid cell size for tableShort: $cellSizeBytes")

    val shortBuff = wrapped.asShortBuffer()
    val toBuff    = new ArrayBuffer[Short](numCells)

    while (shortBuff.hasRemaining) {
      toBuff.addOne(shortBuff.get)
    }

    val equation: Short => Short = EquationParser.parseShortF1(table.axes.z.math.equation)

    toBuff.map { s =>
      val ss = equation(s)
      println(s"ss is $ss")
      ss
    }.toArray
  }
}
