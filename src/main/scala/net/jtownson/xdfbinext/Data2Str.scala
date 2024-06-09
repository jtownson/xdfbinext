package net.jtownson.xdfbinext

object Data2Str {

  def data2StrConst(tableData: String): String = {
    new StringBuilder().append(f"$tableData%9s").append("\n").toString
  }

  def data2StrConst(tableData: BigDecimal): String = {
    new StringBuilder().append(f"$tableData%9s").append("\n").toString
  }

  private def pad(s: String, len: Int): String = {
    val p = len - s.length
    " ".repeat(p) + s
  }

  def data2Str1D(xAxisData: Array[String], tableData: Array[BigDecimal]): String = {
    data2Str1D(xAxisData, tableData.map(_.toString))
  }

  def data2Str1D(xAxisData: Array[String], tableData: Array[String]): String = {
    val cols       = xAxisData.length
    val maxXLen    = xAxisData.map(_.length).max
    val maxDataLen = tableData.map(_.length).max
    val len        = Math.max(maxXLen, maxDataLen) + 1

    val xAxisHeader = (0 until cols).map { col => pad(xAxisData(col), len) }.mkString
    val rowStr      = (0 until cols).map { col => pad(tableData(col), len) }.mkString
    new StringBuilder().append(xAxisHeader).append("\n").append(rowStr).append("\n").toString
  }

  def data2Str2D(
      xAxisData: Array[String],
      yAxisData: Array[String],
      tableData: Array[BigDecimal]
  ): String = {
    data2Str2D(xAxisData, yAxisData, tableData.map(_.toString))
  }

  def data2Str2D(
      xAxisData: Array[String],
      yAxisData: Array[String],
      tableData: Array[String]
  ): String = {
    val cols = xAxisData.length
    val rows = yAxisData.length

    val maxXLen    = xAxisData.map(_.length).max
    val maxYLen    = yAxisData.map(_.length).max
    val maxDataLen = tableData.map(_.length).max
    val len        = Math.max(maxXLen, Math.max(maxYLen, maxDataLen)) + 1

    val sb = new StringBuilder()

    val xAxisHeader = (0 until cols).map { col => pad(xAxisData(col), len) }.mkString
    sb.append(" ".repeat(len)).append(xAxisHeader).append("\n")
    (0 until rows).map { row =>
      sb.append(pad(yAxisData(row), len))
      val rowStr = (0 until cols).map { col =>
        val i = row * cols + col
        pad(tableData(i), len)
      }.mkString
      sb.append(rowStr).append("\n")
    }
    sb.toString
  }

}
