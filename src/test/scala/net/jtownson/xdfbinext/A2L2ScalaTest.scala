package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2LBinAdapterTest.getClass
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Path

class A2L2ScalaTest extends AnyFlatSpec {
  private val originalBin = new File("src/test/resources/00003076501103_original.bin")

  private val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL

  private val a2LWrapper = A2LWrapper(a2lUrl)

  private val a2LBinAdapter = new A2LBinAdapter(originalBin, a2LWrapper)

  private val a2L2Scala = new A2L2Scala(a2LWrapper, Path.of("."))

  it should "generate code for a normal function" in {
    a2L2Scala.fnGen("BMW_MOD_TchDiag_Co_100ms")
  }

  it should "generate code for a parent function" in {
    a2L2Scala.fnGen("Tch_039_004_0")
  }

  it should "generate code to call a function" in {
    a2L2Scala.callGen("BMW_MOD_TchDiag_Co_100ms")
  }
}
