package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.XdfTable

case class ManagedTable[T](xdfTable: XdfTable, data: Array[T]) {

//  val unitsX: String = xdfTable.axes.x.u
}
