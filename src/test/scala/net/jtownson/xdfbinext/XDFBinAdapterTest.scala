package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import scala.io.Source
import scala.util.Using

class XDFBinAdapterTest extends AnyFlatSpec {

  private val xdfModel = XdfParser.parse(Source.fromResource("00003076501103.xdf").mkString)

  private val mapSwitchBaseBin = new File(
    getClass.getClassLoader.getResource("00003076501103_MapSwitchBase.bin").toURI
  )

  private val originalBin = new File(
    getClass.getClassLoader.getResource("00003076501103_original.bin").toURI
  )

  private val mppskBin = new File(
    getClass.getClassLoader.getResource("mppsk.bin").toURI
  )

  private val binAdapter = new XDFBinAdapter(mapSwitchBaseBin, xdfModel)

  private def withBinAdapter(xdfFile: File, binFile: File)(test: XDFBinAdapter => Any): Any = {
    Using.resource(Source.fromFile(xdfFile)) { xdfSource =>
      val xdf        = XdfParser.parse(xdfSource.mkString)
      val binAdapter = new XDFBinAdapter(binFile, xdf)
      test(binAdapter)
    }
  }

  private val modifiedXDFFile = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen1\\00003076501103.xdf"
  )

  it should "output a couple of tables" in withBinAdapter(modifiedXDFFile, originalBin) { binAdapter =>
//    println(MapCompare.table2Str(binAdapter, "BMWtchsp_fac_mf_FilCmprNorm_T"))
//    println(MapCompare.table2Str(binAdapter, "BMWtchsp_fac_FilPRatCmpr_T"))
//    println(MapCompare.table2Str(binAdapter, "BMWtchsp_fac_FilPDyn_C"))
//    println(MapCompare.table2Str(binAdapter, "Maximaler Sollladedruck wegen Diagnose Ladedrucksensor"))
    println(MapCompare.table2Str(binAdapter, "BMWtchsp_rat_p_CmprPmp_T"))
  }

  "BinAdapter" should "read a scalar short" in {
    val expected = Array(240)
    val table    = "Max calculated power"
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read a vector of byte as unsigned, respecting decimal places" in {
    val expected = Array[BigDecimal](1.008, 1.000, 1.000, 1.000, 0.914, 0.852)
    val table    = "Torque Reduction Factor (RPM)"
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read a vector with a strange -ve major stride value" in {
    val expected = Array[BigDecimal](1400, 2600, 3600, 5000, 6000, 6500)
    val table    = "Torque Reduction Factor (RPM) X (autogen)"
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read a vector of short" in {
    val expected = Array[BigDecimal](185, 185, 185, 185, 185, 185, 172, 168, 166, 160, 160, 160)
    val table    = "Max load (spool)"
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read a 2D table of short" in {
    val expected = Array[BigDecimal](0, 200, 320, 353, 423, 450, 0, 200, 320, 353, 423, 450, 0, 200, 320, 353, 423, 450,
      0, 200, 320, 353, 433, 450, 0, 200, 320, 360, 433, 450, 0, 200, 325, 360, 433, 450)

    val table = "Performance gauge scaling"
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read a 2D table of unsigned short values" in {
    val table = "Rail pressure homogen"
    binAdapter.tableRead(table).exists(value => value < 0) shouldBe false
  }

  it should "read a constant with a 32 bit values" in {
    binAdapter.tableRead("Maximum indicated torque (Nm)") shouldBe Array[BigDecimal](BigDecimal("521.00"))
  }

  it should "read timing main breakpoints correctly" in {
    val table = "Timing (Main) (FF#2) - RPM"
    val expected = Array[BigDecimal](500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 4000,
      4500, 5000, 5500, 6000, 6500, 6950)
    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read WGDC P correction X (autogen)" in {
    val table    = "WGDC P correction X (autogen)"
    val expected = Array[BigDecimal](-500.0, -50.0, -20.0, -10.0, -5.0, 0.0, 5.0, 10.0, 30.0, 50.0, 100.0, 500.0)

    binAdapter.tableRead(table) shouldBe expected
  }

  it should "read Miniumum/Maximum fuer CltPrt_MdDec_P" in {
    binAdapter.tableRead("Minimum fuer CltPrt_MdDec_P") shouldBe Array(BigDecimal("-395.000"))
    binAdapter.tableRead("Maximum fuer CltPrt_MdDec_P") shouldBe Array(BigDecimal("495.000"))
  }

  it should "read Timing (main)" in {
    binAdapter.tableRead("Timing (main)").exists(_ < 0) shouldBe true
  }

  it should "read a value from a 1D table before the min breakpoints" in {
    binAdapter.tableRead1D("Max load (spool)").data.atX(1400) shouldBe BigDecimal(185.0)
    binAdapter.tableRead1D("Max load (spool)").data.atX(1500) shouldBe BigDecimal(185.0)
  }

  it should "read a value from a 1D table after the max breakpoint" in {
    binAdapter.tableRead1D("Max load (spool)").data.atX(7500) shouldBe BigDecimal(160.0)
    binAdapter.tableRead1D("Max load (spool)").data.atX(7000) shouldBe BigDecimal(160.0)
  }

  it should "read a value from a 1D table within the breakpoint range" in {
    binAdapter.tableRead1D("Max load (spool)").data.atX(5400) shouldBe BigDecimal(174.6)
    binAdapter.tableRead1D("Max load (spool)").data.atX(5500) shouldBe BigDecimal(172.0)
    binAdapter.tableRead1D("Max load (spool)").data.atX(5600) shouldBe BigDecimal(170.4)
  }

  it should "read a value from a 1D table where the breakpoints are defined by labels" in {
    binAdapter.tableRead1D("Fan PWM based on coolant act to target delta").data.atX(-3.1) shouldBe BigDecimal(0.0)
    binAdapter.tableRead1D("Fan PWM based on coolant act to target delta").data.atX(0.1) shouldBe BigDecimal(1.2)
    binAdapter.tableRead1D("Fan PWM based on coolant act to target delta").data.atX(12.0) shouldBe BigDecimal(68.850)
    binAdapter.tableRead1D("Fan PWM based on coolant act to target delta").data.atX(16.0) shouldBe BigDecimal(100.0)
  }

  it should "read a value fro a 2D table" in {
    /*
    77.0	95.0
    79.0	96.0
     */
    binAdapter.tableRead2D("Load to torque").data.atXY(25, 1800) shouldBe BigDecimal(88.08)
  }
}
