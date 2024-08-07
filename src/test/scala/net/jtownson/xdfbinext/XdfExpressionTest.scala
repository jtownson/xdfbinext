package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.InverseLookup2D
import org.apache.commons.lang3.CharSet
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.io.{BufferedSource, Source}
import scala.math.BigDecimal.RoundingMode
import scala.util.Using

class XdfExpressionTest extends AnyFlatSpec {

  private val xdfFile = Source.fromResource("00003076501103-jmt-expr.xdf.xml").mkString
  private val xdf     = XdfParser.parse(xdfFile)

  private val originalBin = new File(
    getClass.getClassLoader.getResource("00003076501103_original.bin").toURI
  )

  private val binAdapter = new XDFBinAdapter(originalBin, xdf)

  it should "process report" ignore {
    val tables    = xdf.tablesByName.keySet
    val tableExpr = """====\s(.+)\s====""".r
    Using.resource(Source.fromResource("mppsk.txt")) { src =>
      val sb = new StringBuilder()
      src.getLines().foreach { line =>
        println(line)
        line match
          case tableExpr(tableName) =>
            val linkName = tableName.replace(' ', '_')
            println()
            println(s"""'''Ref''': [[DME_table_reference#$linkName|$tableName]]""")
          case _ =>
      }
    }
  }

  "XdfExpression" should "read a virtual table metadata" in {
    // we want to define a model structure which has discoverable input variables
    // the user can then provide values or ranges of values for those variables
    // In the simple case, the variables are input to a table to get a result
    // In a more complex case, the output from one table becomes one of the inputs
    // to another table. This in fact is the useful case because the simple case
    // is dealt with by inspection of the single table in tunerpro.

//    val expectedRpmBreakpointsTable = xdf.tables2D("Load to torque").yAxisBreakpoints

    xdf.virtualTablesByName("Torque to load").title shouldBe "Torque to load"
    xdf.virtualTablesByName("Torque to load").description shouldBe "Inverse lookup into Load to torque"
    xdf.virtualTablesByName("Torque to load").tableDef shouldBe InverseLookup2D("Load to torque", "x")
  }

  it should "read an inverse table" in {
    val tqLd = binAdapter.virtualTableByName("Torque to load").rounded(1)

    tqLd.xAxis.toSeq shouldBe Seq(0, 45, 90, 135, 180, 225, 270, 315, 360, 405, 450, 495, 540, 585, 630, 675, 720, 765)
    tqLd.yAxis.toSeq shouldBe Seq(500, 600, 800, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3500, 4000, 4500,
      5000, 5500, 6000, 6500, 7000)
    tqLd.atXY( /* torque */ 450, /* rpm */ 4500) should equal(BigDecimal(117.4) +- 1)
  }

  it should "read a table defined from arithmetic on other tables" in {
    val lobeSep = binAdapter.virtualTableByName("Mean lobe angle (warm) 1")

    val vanosInW1 = binAdapter.tableRead2D("Vanos IN (warm) 1")
    val vanosExW1 = binAdapter.tableRead2D("Vanos EX (warm) 1")
    val expected  = vanosInW1.data.values.zip(vanosExW1.data.values).map((vi, ve) => (vi + ve) / 2)

    lobeSep.xAxis.toSeq shouldBe vanosInW1.data.xAxis.toSeq
    lobeSep.yAxis.toSeq shouldBe vanosInW1.data.yAxis.toSeq
    lobeSep.values.toSeq shouldBe expected.toSeq
  }

  it should "simulate D component" ignore {
    val dCorrection = binAdapter.tableRead1D("WGDC D correction").data
    val dFactor     = binAdapter.tableRead2D("WGDC D-Factor").data

    val boostError         = (-400 to 400) by 40
    val boostErrorGradient = (-30 to 30) by 10
    val compressorPower    = (1 to 25) by 3

    def dFactorCalc(boostError: BigDecimal, boostErrorGradient: BigDecimal, compressorPower: BigDecimal): BigDecimal = {
      (dFactor.atXY(boostError, boostErrorGradient) * dCorrection.atX(compressorPower))
        .setScale(3, RoundingMode.HALF_UP)
    }
    println(s"Boost Error (hPa),Boost Error Gradient (hPa),Compressor Power (kW)")
    for {
      be  <- boostError
      beg <- boostErrorGradient
      cp  <- compressorPower
    } {
      println(s"$be,$beg,$cp,${dFactorCalc(be, beg, cp)}")
    }
  }
}
