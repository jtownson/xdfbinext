package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.{AxisDescr, AxisPtsXYZ45, Characteristic, FncValues, NoAxisPtsXYZ45}
import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.enums.DataType.*
import net.jtownson.xdfbinext.a2l.PositionWrapper.sizeOf

trait PositionWrapper {
  val position: Int
  val cellCount: Int
  val isAxis: Boolean
  val isFncValues: Boolean
  val dataType: DataType

  def cellSize: Int = sizeOf(dataType)

  def byteCount: Int = cellCount * cellSize
}

object PositionWrapper {

  private val sizeOf: Map[DataType, Int] = Map(
    SBYTE -> 1,
    UBYTE -> 1,
    SWORD -> 2,
    UWORD -> 2,
    SLONG -> 4,
    ULONG -> 4,
    FLOAT32_IEEE -> 4,
    FLOAT64_IEEE -> 8
  )

  case class NoAxPtsPosition(noAxisPts: NoAxisPtsXYZ45) extends PositionWrapper {
    val position: Int = noAxisPts.getPosition.toInt
    val cellCount: Int = 1
    val isAxis: Boolean = false
    val isFncValues: Boolean = false
    val dataType: DataType = noAxisPts.getDataType
  }

  case class AxPtsPosition(axDesc: AxisDescr, axisPts: AxisPtsXYZ45) extends PositionWrapper {
    val position: Int = axisPts.getPosition.toInt
    val cellCount: Int = axDesc.getMaxAxisPoints.toInt
    val isAxis: Boolean = true
    val isFncValues: Boolean = false
    val dataType: DataType = axisPts.getDatatype
  }

  case class FncValuesPosition(c: Characteristic, axDesc: AxisDescr, fncValues: FncValues) extends PositionWrapper {
    val position: Int = fncValues.getPosition.toInt
    val cellCount: Int = axDesc.getMaxAxisPoints.toInt
    val isAxis: Boolean = false
    val isFncValues: Boolean = true
    val dataType: DataType = fncValues.getDataType
  }

  case class FncValuesPositionXY(c: Characteristic, xAxDesc: AxisDescr, yAxDesc: AxisDescr, fncValues: FncValues)
    extends PositionWrapper {
    val position: Int = fncValues.getPosition.toInt
    val cellCount: Int = xAxDesc.getMaxAxisPoints.toInt * yAxDesc.getMaxAxisPoints.toInt
    val isAxis: Boolean = false
    val isFncValues: Boolean = true
    val dataType: DataType = fncValues.getDataType
  }
}
