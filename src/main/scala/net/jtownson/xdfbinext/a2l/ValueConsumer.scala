package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.{Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.ByteBlock.fnLabel
import net.jtownson.xdfbinext.a2l.BlockConsumer.toTypedConsumableRecord

import java.io.RandomAccessFile

class ValueConsumer(z: BlockConsumer) {
  def applyFuncFormula(ratFun: RatFun, dp: Int): BigDecimal =
    z.applyFormula(ratFun, dp).head

  def applyFuncVTab(vtab: CompuVTab): String =
    z.applyVTab(vtab).head

  def applyFuncTab(tab: CompuTab): BigDecimal =
    z.applyTab(tab).head
}

object ValueConsumer {

  type ValueType = BigDecimal | String

  def foldValueType[T](fBd: BigDecimal => T, fStr: String => T)(value: ValueType): T = value match {
    case bd: BigDecimal =>
      fBd(bd)
    case s: String =>
      fStr(s)
  }

  def apply(
      c: Characteristic,
      fnLayout: RecordLayout,
      offset: Long,
      binAccess: RandomAccessFile
  ): ValueConsumer = {

    val cRec = toTypedConsumableRecord(
      c.getAddress - offset,
      1,
      1,
      fnLayout,
      binAccess
    )

    val z = cRec(fnLabel)

    new ValueConsumer(z)
  }
}
