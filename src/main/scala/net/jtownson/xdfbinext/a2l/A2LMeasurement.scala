package net.jtownson.xdfbinext.a2l

import breeze.io.RandomAccessFile
import net.alenzen.a2l.Measurement
import net.alenzen.a2l.enums.DataType
import net.jtownson.xdfbinext.A2LBinAdapter
import net.jtownson.xdfbinext.A2LBinAdapter.FileBackedSegment

trait A2LMeasurement {
  def read1: BigDecimal

  def read: Array[BigDecimal]

  def write1(bd: BigDecimal): Unit

  def write(bds: Array[BigDecimal]): Unit
}

object A2LMeasurement {

  type InMeasurement  = A2LMeasurement
  type OutMeasurement = A2LMeasurement

  case class TmpMeasurement1(var v: BigDecimal) extends A2LMeasurement {

    override def read1: BigDecimal = v

    override def read: Array[BigDecimal] = throw new UnsupportedOperationException()

    override def write1(bd: BigDecimal): Unit = v = bd

    override def write(bds: Array[BigDecimal]): Unit = throw new UnsupportedOperationException()
  }

  case class TmpMeasurementN(var vs: Array[BigDecimal]) extends A2LMeasurement {

    override def read1: BigDecimal = throw new UnsupportedOperationException()

    override def read: Array[BigDecimal] = vs

    override def write1(bd: BigDecimal): Unit = throw new UnsupportedOperationException()

    override def write(bds: Array[BigDecimal]): Unit = vs = bds
  }

  case class BinMeasurement(measurement: Measurement, a2lBin: A2LBinAdapter) extends A2LMeasurement {

    val dataType: DataType            = measurement.getDatatype
    val length: Int                   = a2lBin.a2l.getXDim(measurement)
    val ramSegment: FileBackedSegment = a2lBin.ramSegments(measurement)
    val memoryFile: RandomAccessFile  = ramSegment.file
    val offset: Long                  = ramSegment.getCheckedOffset(measurement.getEcuAddress)

    require(
      a2lBin.a2l.getYDim(measurement) == 1 && a2lBin.a2l.getZDim(measurement) == 1,
      s"Unsupported multidimensional array measurement $measurement"
    )

    def read1: BigDecimal = {
      require(length == 1)
      a2lBin.readMeasurement(dataType, memoryFile, offset, length).head
    }

    def read: Array[BigDecimal] =
      a2lBin.readMeasurement(dataType, memoryFile, offset, length)

    def write1(bd: BigDecimal): Unit = {
      write(Array(bd))
    }

    def write(bds: Array[BigDecimal]): Unit = {
      require(bds.length == length)
      a2lBin.writeMeasurement(bds, dataType, memoryFile, offset, length)
    }
  }
}
