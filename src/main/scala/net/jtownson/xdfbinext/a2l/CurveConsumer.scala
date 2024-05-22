package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.{AxisDescr, AxisPts, Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.ByteBlock.{xLabel, fnLabel}
import net.jtownson.xdfbinext.a2l.BlockConsumer.toTypedConsumableRecord

import java.io.RandomAccessFile

class CurveConsumer(x: BlockConsumer, z: BlockConsumer) {
  def applyAxisFormula(ratFun: RatFun, dp: Int): NumericArray =
    NumericArray(x.applyFormula(ratFun, dp))

  def applyAxisVTab(vtab: CompuVTab): StringArray =
    StringArray(x.applyVTab(vtab))

  def applyAxisTab(tab: CompuTab): NumericArray =
    NumericArray(x.applyTab(tab))

  def applyFuncFormula(ratFun: RatFun, dp: Int): NumericArray =
    NumericArray(z.applyFormula(ratFun, dp))

  def applyFuncVTab(vtab: CompuVTab): StringArray =
    StringArray(z.applyVTab(vtab))

  def applyFuncTab(tab: CompuTab): NumericArray =
    NumericArray(z.applyTab(tab))
}

object CurveConsumer {

  def apply(
      c: Characteristic,
      axisType: DataType,
      axisPts: AxisPts,
      axisLayout: RecordLayout,
      fnLayout: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): CurveConsumer = {

    val xRec = toTypedConsumableRecord(
      axisPts.getAddress - offset,
      axisPts.getMaxAxisPoints.toInt,
      1,
      axisLayout,
      binAccess
    )

    val x = xRec(xLabel)

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      axisPts.getMaxAxisPoints.toInt,
      1,
      fnLayout,
      binAccess
    )

    val z = cRec(fnLabel)

    new CurveConsumer(x, z)
  }

  def apply(
      c: Characteristic,
      axisDescr: AxisDescr,
      r: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): CurveConsumer = {

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      axisDescr.getMaxAxisPoints.toInt,
      1,
      r,
      binAccess
    )

    val x = cRec(xLabel)

    val z = cRec(fnLabel)

    new CurveConsumer(x, z)
  }
}
