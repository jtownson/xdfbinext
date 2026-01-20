package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2LBinAdapterTest.getClass
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{File, PrintStream}
import java.nio.file.Path
import scala.util.Using

class A2L2ScalaTest extends AnyFlatSpec {
  private val originalBin = new File("src/test/resources/00003076501103_original.bin")

  private val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL

  private val a2LWrapper = A2LWrapper(a2lUrl)

  private val a2LBinAdapter = new A2LBinAdapter(originalBin, a2LWrapper)

  private val a2L2Scala = new A2L2Scala(a2LWrapper, Path.of("."))

  it should "generate code for Tch_039_004_0" in withSrcFile("Tch_039_004_0.scala") { out =>
    a2L2Scala.fnGen("Tch_039_004_0", out)
  }

  it should "generate code for Tqw_111_001_0" in withSrcFile("Tqw_111_001_0.scala") { out =>
    a2L2Scala.fnGen("Tqw_111_001_0", out)
  }

  def withSrcFile(fn: String)(use: PrintStream => Any): Unit = {
    val f = new File(s"src/main/scala/net/jtownson/xdfbinext/bace/DME861_R1C9J8B3B/$fn")
    Using.resource(new PrintStream(f))(out => use(out))
  }

}
