package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.{AxisDescr, AxisPts, Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.BlockConsumer.toTypedConsumableRecord
import net.jtownson.xdfbinext.a2l.ByteBlock.{fnLabel, xLabel, yLabel}

import java.io.RandomAccessFile

class MapConsumer(x: BlockConsumer, y: BlockConsumer, z: BlockConsumer) {
  def applyXAxisFormula(ratFun: RatFun, dp: Int): NumericArray = NumericArray(x.applyFormula(ratFun, dp))

  def applyXAxisVTab(vTab: CompuVTab): StringArray = StringArray(x.applyVTab(vTab))

  def applyXAxisTab(tab: CompuTab): NumericArray = NumericArray(x.applyTab(tab))

  def applyYAxisFormula(ratFun: RatFun, dp: Int): NumericArray = NumericArray(y.applyFormula(ratFun, dp))

  def applyYAxisVTab(vTab: CompuVTab): StringArray = StringArray(y.applyVTab(vTab))

  def applyYAxisTab(tab: CompuTab): NumericArray = NumericArray(y.applyTab(tab))

  def applyFuncFormula(ratFun: RatFun, dp: Int): NumericArray = NumericArray(z.applyFormula(ratFun, dp))

  def applyFuncVTab(vTab: CompuVTab): StringArray = StringArray(z.applyVTab(vTab))

  def applyFuncTab(tab: CompuTab): NumericArray = NumericArray(z.applyTab(tab))
}

object MapConsumer {

  def apply(
      c: Characteristic,
      xAxisType: DataType,
      xAxisPts: AxisPts,
      xAxisLayout: RecordLayout,
      yAxisType: DataType,
      yAxisPts: AxisPts,
      yAxisLayout: RecordLayout,
      fnLayout: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): MapConsumer = {

    val xRec = toTypedConsumableRecord(
      xAxisPts.getAddress - offset,
      xAxisPts.getMaxAxisPoints.toInt,
      1,
      xAxisLayout,
      binAccess
    )

    val x = xRec(xLabel)

    val yRec = toTypedConsumableRecord(
      yAxisPts.getAddress - offset,
      yAxisPts.getMaxAxisPoints.toInt,
      1,
      yAxisLayout,
      binAccess
    )

    val y = yRec(xLabel)

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      xAxisPts.getMaxAxisPoints.toInt,
      yAxisPts.getMaxAxisPoints.toInt,
      fnLayout,
      binAccess
    )

    val z = cRec(fnLabel)

    new MapConsumer(x, y, z)
  }

  def apply(
      c: Characteristic,
      xAxisDescr: AxisDescr,
      yAxisDescr: AxisDescr,
      r: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): MapConsumer = {

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      xAxisDescr.getMaxAxisPoints.toInt,
      yAxisDescr.getMaxAxisPoints.toInt,
      r,
      binAccess
    )

    val x = cRec(xLabel) // with axis pts, the axis values are the function values

    val y = cRec(yLabel)

    val z = cRec(fnLabel)

    new MapConsumer(x, y, z)
  }
}
