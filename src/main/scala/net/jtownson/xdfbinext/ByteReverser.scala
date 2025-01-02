package net.jtownson.xdfbinext

import scala.annotation.tailrec

object ByteReverser {

  @tailrec
  def reverse(in: Int, n: Int = 8, out: Int = 0): Int =
    if (n == 0) out
    else reverse(in >>> 1, n - 1, (out << 1) | (in & 1))

}
