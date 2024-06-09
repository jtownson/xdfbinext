package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.{Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.BlockConsumer.toTypedConsumableRecord
import net.jtownson.xdfbinext.a2l.ByteBlock.fnLabel

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

  type ValBlkType = NumericArray | StringArray

  def foldValBlkType[T](fna: NumericArray => T, fsa: StringArray => T)(value: ValBlkType): T = value match
    case v: NumericArray =>
      fna(v)
    case v: StringArray =>
      fsa(v)

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
