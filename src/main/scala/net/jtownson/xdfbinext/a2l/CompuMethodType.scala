package net.jtownson.xdfbinext.a2l

type CompuMethodType = RatFun | CompuVTab | CompuTab

def compuMethodCata[T](frf: RatFun => T, fvt: CompuVTab => T, fct: CompuTab => T)(cm: CompuMethodType): T = cm match {
  case rf: RatFun =>
    frf(rf)
  case cvt: CompuVTab =>
    fvt(cvt)
  case ct: CompuTab =>
    fct(ct)
}
