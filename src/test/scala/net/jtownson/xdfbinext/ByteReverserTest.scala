package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.ByteReverser.reverse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ByteReverserTest extends AnyFlatSpec {

  behavior of "ByteReverser"

  it should "be consistent" in {
    reverse(0xaa) shouldBe 0x55
    reverse(0xc9) shouldBe 0x93
    for (b <- 0x00 to 0xff) reverse(reverse(b)) shouldBe b
  }
}
