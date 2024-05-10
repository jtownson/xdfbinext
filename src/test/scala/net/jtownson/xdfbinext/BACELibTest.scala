package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.BACELib.bace_lowpass_constiv
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import scala.util.Random

class BACELibTest extends AnyFlatSpec {

  behavior of "BASELib"

  it should "not modify a constant" in {
    val data = List[BigDecimal](1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    bace_lowpass_constiv(data, 1) shouldBe data
  }

  val dataWithNoise = Seq.fill(100)(Random.between(-2, 2)).map(i => BigDecimal(30) + i)
  it should "filter data with noise" ignore {
    val filtered = bace_lowpass_constiv(dataWithNoise, 0.03)
    dataWithNoise.zip(filtered).foreach((l, r) => println(s"$r"))
  }
}
