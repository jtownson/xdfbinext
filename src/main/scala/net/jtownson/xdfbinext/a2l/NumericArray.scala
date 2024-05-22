package net.jtownson.xdfbinext.a2l

case class NumericArray(values: Array[BigDecimal]) {
  override def equals(obj: Any): Boolean = {
    obj match
      case NumericArray(v) =>
        v.sameElements(values)
      case _ =>
        false
  }
}
