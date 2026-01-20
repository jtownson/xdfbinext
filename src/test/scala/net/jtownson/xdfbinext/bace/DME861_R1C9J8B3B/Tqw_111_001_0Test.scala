package net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B

import net.jtownson.xdfbinext.{A2LBinAdapter, A2LWrapper}
import net.jtownson.xdfbinext.A2LBinAdapterTest.getClass
import net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B.Tqw_111_001_0Test.a2LBinAdapter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File

class Tqw_111_001_0Test extends AnyFlatSpec {

  behavior of "Tqw_111_001_0"

  it should "invoke BMW_MOD_Mafw_PreCond" ignore {

    val Status_usecase_antr = a2LBinAdapter.measurement("Status_usecase_antr")
    val Status_usecase_mafw = a2LBinAdapter.measurement("Status_usecase_mafw")

    Status_usecase_antr.write1(BigDecimal(1))
    Status_usecase_mafw.write1(BigDecimal(2))

    Tqw_111_001_0.BMW_MOD_Mafw_PreCond(a2LBinAdapter)

    a2LBinAdapter.measurement("Status_usecase_antr_lim").read1 shouldBe BigDecimal(1)
  }
}

object Tqw_111_001_0Test {
  private val originalBin = new File("src/test/resources/00003076501103_original.bin")

  private val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL

  private val a2LWrapper = A2LWrapper(a2lUrl)

  private val a2LBinAdapter = new A2LBinAdapter(originalBin, a2LWrapper)
}
