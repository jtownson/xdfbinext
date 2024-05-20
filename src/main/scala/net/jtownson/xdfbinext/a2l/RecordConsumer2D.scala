package net.jtownson.xdfbinext.a2l

import net.alenzen.a2l.enums.DataType
import net.alenzen.a2l.{AxisDescr, AxisPts, Characteristic, RecordLayout}
import net.jtownson.xdfbinext.a2l.PositionWrapper.{AxPtsPosition, FncValuesPositionXY, NoAxPtsPosition}
import net.jtownson.xdfbinext.a2l.RatFunFormula.RatFun
import net.jtownson.xdfbinext.a2l.and

import java.io.RandomAccessFile

trait RecordConsumer2D {
  def applyXAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyYAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal]

  def applyFuncVTab(vTab: CompuVTab): Array[String]

  def applyFuncTab(tab: CompuTab): Array[BigDecimal]
}

object RecordConsumer2D {
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
           ): RecordConsumer2D = {
    val ax = ValueConsumer(
      xAxisType,
      xAxisPts.getAddress - offset,
      xAxisPts.getMaxAxisPoints.toInt,
      binAccess
    )
    val ay = ValueConsumer(
      yAxisType,
      yAxisPts.getAddress - offset,
      yAxisPts.getMaxAxisPoints.toInt,
      binAccess
    )
    val f = ValueConsumer(
      fnLayout.getFunctionValues.getDataType,
      c.getAddress - offset,
      xAxisPts.getMaxAxisPoints.toInt * yAxisPts.getMaxAxisPoints.toInt,
      binAccess
    )

    new RecordConsumer2D {
      override def applyXAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = ax.applyFormula(ratFun, dp)

      override def applyYAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = ay.applyFormula(ratFun, dp)

      override def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = f.applyFormula(ratFun, dp)

      override def applyFuncVTab(vTab: CompuVTab): Array[String] = f.applyVTab(vTab)

      override def applyFuncTab(tab: CompuTab): Array[BigDecimal] = f.applyTab(tab)
    }
  }

  def apply(
             c: Characteristic,
             xAxisDescr: AxisDescr,
             yAxisDescr: AxisDescr,
             r: RecordLayout,
             offset: Long,
             binAccess: RandomAccessFile
           ): RecordConsumer2D = {

    val baseAddr = c.getAddress - offset

    Option(r.getNoAxisPtsX).and(Option(r.getAxisPtsX)).and(Option(r.getNoAxisPtsY)).and(Option(r.getAxisPtsY)) match
      case Some((((nx, ax), ny), ay)) =>
        val naxPtsWrapper = NoAxPtsPosition(nx)
        val axPtsWrapper = AxPtsPosition(xAxisDescr, ax)
        val nayPtsWrapper = NoAxPtsPosition(ny)
        val ayPtsWrapper = AxPtsPosition(yAxisDescr, ay)
        val fncValuesWrapper = FncValuesPositionXY(c, xAxisDescr, yAxisDescr, r.getFunctionValues)

        val posSorted: Seq[PositionWrapper] =
          Seq(naxPtsWrapper, axPtsWrapper, nayPtsWrapper, ayPtsWrapper, fncValuesWrapper).sortBy(_.position)
        // format: off
        val offsets: Seq[Long] =
          Seq(
            baseAddr,
            baseAddr + posSorted(0).byteCount,
            baseAddr + posSorted(0).byteCount + posSorted(1).byteCount,
            baseAddr + posSorted(0).byteCount + posSorted(1).byteCount + posSorted(2).byteCount,
            baseAddr + posSorted(0).byteCount + posSorted(1).byteCount + posSorted(2).byteCount + posSorted(3).byteCount
          )
        // format: on

        val readers = Seq(
          ValueConsumer(posSorted(0).dataType, offsets(0), posSorted(0).cellCount, binAccess),
          ValueConsumer(posSorted(1).dataType, offsets(1), posSorted(1).cellCount, binAccess),
          ValueConsumer(posSorted(2).dataType, offsets(2), posSorted(2).cellCount, binAccess),
          ValueConsumer(posSorted(3).dataType, offsets(3), posSorted(3).cellCount, binAccess),
          ValueConsumer(posSorted(4).dataType, offsets(4), posSorted(4).cellCount, binAccess)
        )

        def findAxis(axisDescr: AxisDescr)(pw: PositionWrapper): Boolean = pw match {
          case AxPtsPosition(desc, pts) =>
            desc == axisDescr
          case _ =>
            false
        }

        val xAxisReader = readers(posSorted.indexWhere(findAxis(xAxisDescr)))
        val yAxisReader = readers(posSorted.indexWhere(findAxis(yAxisDescr)))
        val fncReader = readers(posSorted.indexWhere(_.isFncValues))

        new RecordConsumer2D {
          override def applyXAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
            xAxisReader.applyFormula(ratFun, dp)
          }

          override def applyYAxisFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] = {
            yAxisReader.applyFormula(ratFun, dp)
          }

          override def applyFuncFormula(ratFun: RatFun, dp: Int): Array[BigDecimal] =
            fncReader.applyFormula(ratFun, dp)

          override def applyFuncVTab(vTab: CompuVTab): Array[String] =
            fncReader.applyVTab(vTab)

          override def applyFuncTab(tab: CompuTab): Array[BigDecimal] =
            fncReader.applyTab(tab)

        }

      case None =>
        throw new IllegalArgumentException("Not the right axis type")
  }
}
