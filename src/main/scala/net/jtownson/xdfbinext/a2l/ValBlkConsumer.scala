package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.{AxisDescr, AxisPts, Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.ByteBlock.{fnLabel, xLabel, yLabel}
import net.jtownson.xdfbinext.a2l.BlockConsumer.toTypedConsumableRecord

import java.io.RandomAccessFile

class ValBlkConsumer(z: BlockConsumer) {

  def applyFuncFormula(ratFun: RatFun, dp: Int): NumericArray =
    NumericArray(z.applyFormula(ratFun, dp))

  def applyFuncVTab(vtab: CompuVTab): StringArray =
    StringArray(z.applyVTab(vtab))

  def applyFuncTab(tab: CompuTab): NumericArray =
    NumericArray(z.applyTab(tab))
}

object ValBlkConsumer {

  type ValueBlkType = NumericArray | StringArray

  def apply(
      c: Characteristic,
      fnLayout: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): ValBlkConsumer = {

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      c.getNumber.toInt,
      1,
      fnLayout,
      binAccess
    )

    val z = cRec(fnLabel)

    new ValBlkConsumer(z)
  }
}
