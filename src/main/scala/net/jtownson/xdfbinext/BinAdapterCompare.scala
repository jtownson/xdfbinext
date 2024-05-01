package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.Data2Str.{data2Str1D, data2Str2D, data2StrConst}
import net.jtownson.xdfbinext.XdfSchema.{XdfModel, XdfTable, XdfTable1D, XdfTable2D}

import java.io.File

case class BinAdapterCompare(lhs: String, diff: String, rhs: String)

object BinAdapterCompare {

  def compare(xdfModel: XdfModel, lhs: File, rhs: File): Map[String, BinAdapterCompare] = {
    val lhsb = new XDFBinAdapter(lhs, xdfModel)
    val rhsb = new XDFBinAdapter(rhs, xdfModel)

    xdfModel.tablesByName.keySet
      .map(tableName => tableName -> compare(tableName, xdfModel, lhsb, rhsb))
      .toMap
      .collect { case (name, Some(comparison)) => name -> comparison }
  }

  private def compare(
      tableName: String,
      xdfModel: XdfModel,
      lhsb: XDFBinAdapter,
      rhsb: XDFBinAdapter
  ): Option[BinAdapterCompare] = {
    val table = xdfModel.tablesByName(tableName)

    val tl = lhsb.tableReadStr(tableName)
    val tr = rhsb.tableReadStr(tableName)

    val dl = lhsb.tableDyn(table)
    val dr = rhsb.tableDyn(table)

    val diff = rhsb.applyDecimalPl(table)(dl.zip(dr).map { case (lbd, rbd) => rbd - lbd })

    xdfModel.table(tableName) match
      case t: XdfTable =>
        if (dl.sameElements(dr)) {
          None
        } else {
          Some(BinAdapterCompare(tl, data2StrConst(diff.head), tr))
        }
      case t: XdfTable1D =>
        val xLh = t.xAxisBreakpoints.map(x => lhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        val xRh = t.xAxisBreakpoints.map(x => rhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        if (dl.sameElements(dr) && xLh.sameElements(xRh)) {
          None
        } else {
          val td = data2Str1D(rhsb.tableReadOrX(t.table, t.xAxisBreakpoints), diff)
          Some(BinAdapterCompare(tl, td, tr))
        }
      case t: XdfTable2D =>
        val xLh = t.xAxisBreakpoints.map(x => lhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])
        val xRh = t.xAxisBreakpoints.map(x => rhsb.tableDyn(x)).getOrElse(Array.empty[BigDecimal])

        val yLh = t.yAxisBreakpoints.map(y => lhsb.tableDyn(y)).getOrElse(Array.empty[BigDecimal])
        val yRh = t.yAxisBreakpoints.map(y => rhsb.tableDyn(y)).getOrElse(Array.empty[BigDecimal])
        if (dl.sameElements(dr) && xLh.sameElements(xRh) && yLh.sameElements(yRh)) {
          None
        } else {
          val td = data2Str2D(
            rhsb.tableReadOrX(t.table, t.xAxisBreakpoints),
            rhsb.tableReadOrY(t.table, t.yAxisBreakpoints),
            diff
          )
          Some(BinAdapterCompare(tl, td, tr))
        }
  }
}
