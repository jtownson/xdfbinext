package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XDFBinAdapter.BinTable2D
import net.jtownson.xdfbinext.XdfSchema.XdfTable2D

class GraphAdapter2D(data: Interpolated2D, xLabel: String, yLabel: String, zLabel: String) {
//  def asSvg()
}

object GraphAdapter2D {
  def apply(t: BinTable2D): GraphAdapter2D = {
    new GraphAdapter2D(
      t.data,
      t.xdfTable.table.xUnits,
      t.xdfTable.table.yUnits,
      s"${t.xdfTable.table.title} (${t.xdfTable.table.zUnits}"
    )
  }

  private def xLabel(t: XdfTable2D): String = {
    t.table.xUnits
  }
}
