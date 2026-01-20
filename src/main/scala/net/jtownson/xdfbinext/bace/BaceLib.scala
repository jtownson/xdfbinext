package net.jtownson.xdfbinext.bace

import net.jtownson.xdfbinext.bace.BaceDSL.{Block31, Input}

object BaceLib {

//  def ifThenElse[O]: Block31[Boolean, O, O, O] = new Block31 {
//    override def apply(condition: Boolean, trueValue: O, falseValue: O): O =
//      if condition then trueValue
//      else falseValue
//  }

//  def identity1[I: Input]:
  def ifThenElse[I1: Input, I2: Input](using ev: Conversion[I1, Boolean]): Block31[I1, I2, I2, I2] = new Block31 {
    override def apply(condition: I1, trueValue: I2, falseValue: I2): I2 =
      if ev(condition) then trueValue
      else falseValue
  }

  //  y(i) = K*(u(i)-y(i-1)) + y(i-1)
  def bace_lowpass_constiv(u: Seq[BigDecimal], k: BigDecimal): Seq[BigDecimal] = {
    def loop(y: Vector[BigDecimal], i: Int): Vector[BigDecimal] = {
      if (i == u.length) {
        y
      } else if (i == 0) {
        loop(y :+ u(i), i + 1)
      } else {
        val yi = k * (u(i) - y(i - 1)) + y(i - 1)
        loop(y :+ yi, i + 1)
      }
    }
    loop(Vector.empty, 0)
  }
}
