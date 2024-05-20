package net.jtownson.xdfbinext.a2l

extension [T](l: Option[T]) {
  def and[U](r: Option[U]): Option[(T, U)] = for {
    lo <- l
    ro <- r
  } yield (lo, ro)
}
