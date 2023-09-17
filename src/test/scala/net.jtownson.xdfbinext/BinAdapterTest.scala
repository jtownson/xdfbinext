package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.File
import scala.io.Source

class BinAdapterTest extends AnyFlatSpec:

  private val xdfModel = XdfParser.parse(Source.fromResource("00003076501103.xdf").mkString)
  private val binFile = new File(
    getClass.getClassLoader.getResource("00003076501103_MapSwitchBase.bin").toURI
  )
  private val binAdapter = new BinAdapter(binFile, xdfModel)

  "BinAdapter" should "read a scalar table of short" in {
    binAdapter.tableShort("Max calculated power") shouldBe Array(240)
  }
