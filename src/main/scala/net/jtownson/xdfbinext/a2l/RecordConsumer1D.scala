package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.{AxisDescr, AxisPts, Characteristic, RecordLayout}
import net.alenzen.a2l.enums.DataType
import net.jtownson.xdfbinext.a2l.{CompuTab, CompuVTab}
import net.jtownson.xdfbinext.a2l.PositionWrapper.{AxPtsPosition, FncValuesPosition, NoAxPtsPosition}
import net.jtownson.xdfbinext.a2l.RatFunFormula.RatFun
import net.jtownson.xdfbinext.a2l.and
import java.io.RandomAccessFile

trait RecordConsumer1D {
  def applyAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyAxisVTab(vtab: CompuVTab): Array[String]

  def applyAxisTab(tab: CompuTab): Array[BigDecimal]

  def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyFuncVTab(vtab: CompuVTab): Array[String]

  def applyFuncTab(tab: CompuTab): Array[BigDecimal]
}

object RecordConsumer1D {

  def apply(
             c: Characteristic,
             axisType: DataType,
             axisPts: AxisPts,
             axisLayout: RecordLayout,
             fnLayout: RecordLayout,
             offset: Long,
             binAccess: RandomAccessFile
           ): RecordConsumer1D = {
    val ax = ValueConsumer(
      axisType,
      axisPts.getAddress - offset,
      axisPts.getMaxAxisPoints.toInt,
      binAccess
    )
    val f = ValueConsumer(
      fnLayout.getFunctionValues.getDataType,
      c.getAddress - offset,
      axisPts.getMaxAxisPoints.toInt,
      binAccess
    )

    new RecordConsumer1D {
      override def applyAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = ax.applyFormula(ratFun, dp)

      override def applyAxisVTab(vtab: CompuVTab): Array[String] = ax.applyVTab(vtab)

      override def applyAxisTab(tab: CompuTab): Array[BigDecimal] = ax.applyTab(tab)

      override def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = f.applyFormula(ratFun, dp)

      override def applyFuncVTab(vtab: CompuVTab): Array[String] = f.applyVTab(vtab)

      override def applyFuncTab(tab: CompuTab): Array[BigDecimal] = f.applyTab(tab)
    }
  }

  def apply(
             c: Characteristic,
             axisDescr: AxisDescr,
             r: RecordLayout,
             offset: Long,
             binAccess: RandomAccessFile
           ): RecordConsumer1D = {

    val baseAddr = c.getAddress - offset

    Option(r.getNoAxisPtsX).and(Option(r.getAxisPtsX)) match
      case Some((nx, ax)) =>
        val naxPtsWrapper = NoAxPtsPosition(nx)
        val axPtsWrapper = AxPtsPosition(axisDescr, ax)
        val fncValuesWrapper = FncValuesPosition(c, axisDescr, r.getFunctionValues)

        val posSorted: Seq[PositionWrapper] = Seq(naxPtsWrapper, axPtsWrapper, fncValuesWrapper).sortBy(_.position)

        val offsets: Seq[Long] =
          Seq(baseAddr, baseAddr + posSorted(0).byteCount, baseAddr + posSorted(0).byteCount + posSorted(1).byteCount)

        val readers = Seq(
          ValueConsumer(posSorted(0).dataType, offsets(0), posSorted(0).cellCount, binAccess),
          ValueConsumer(posSorted(1).dataType, offsets(1), posSorted(1).cellCount, binAccess),
          ValueConsumer(posSorted(2).dataType, offsets(2), posSorted(2).cellCount, binAccess)
        )

        val axisReader = readers(posSorted.indexWhere(_.isAxis))
        val fncReader = readers(posSorted.indexWhere(_.isFncValues))

        new RecordConsumer1D {
          override def applyAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
            axisReader.applyFormula(ratFun, dp)
          }

          override def applyAxisVTab(vtab: CompuVTab): Array[String] =
            axisReader.applyVTab(vtab)

          override def applyAxisTab(tab: CompuTab): Array[BigDecimal] =
            axisReader.applyTab(tab)

          override def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
            fncReader.applyFormula(ratFun, dp)

          override def applyFuncVTab(vtab: CompuVTab): Array[String] =
            fncReader.applyVTab(vtab)

          override def applyFuncTab(tab: CompuTab): Array[BigDecimal] =
            fncReader.applyTab(tab)
        }

      case None =>
        throw new IllegalArgumentException("Not the right axis type")

  }
}
