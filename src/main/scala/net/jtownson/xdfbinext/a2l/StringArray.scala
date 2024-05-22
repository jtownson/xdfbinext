package net.jtownson.xdfbinext.a2l

case class StringArray(values: Array[String]) {
  override def equals(obj: Any): Boolean = {
    obj match
      case StringArray(v) =>
        v.sameElements(values)
      case _ =>
        false
  }
}
