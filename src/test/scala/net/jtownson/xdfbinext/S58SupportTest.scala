package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import scala.io.Source
import scala.util.Using

class S58SupportTest extends AnyFlatSpec {

  val xdfFile = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\MHD BMW Maps\\F-G Series\\s58_____MG1CS024 F4C9G595B+damos\\00005c640a6405_____MG1CS024 F4C9G595B\\00005c640a6405.xdf"
  )
  val bin1 = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\MHD BMW Maps\\F-G Series\\s58_____MG1CS024 F4C9G595B+damos\\00005c640a6405_____MG1CS024 F4C9G595B\\v102 stage 2 95oct_102ron.bin"
  )

  private val xdfModel   = Using.resource(Source.fromFile(xdfFile))(r => XdfParser.parse(r.mkString))
  private val binAdapter = new XDFBinAdapter(bin1, xdfModel)

  it should "work for LSB short case, boost set limit" in {
    val expectedAxis = Array[BigDecimal](138.9, 148.6, 166.7, 180.6, 201.4, 222.2, 277.8, 291.9)
    val expectedData = Array[BigDecimal](4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000, 4.000)

    val tableRead = binAdapter.tableRead1D("Boost set limit")

    tableRead.data.axis shouldBe expectedAxis
    tableRead.data.values shouldBe expectedData
  }

  it should "work for Boost target offset (sport)" in {
    val expectedAxisX = Array[BigDecimal](2000, 2500, 3000, 4000, 5000, 6000)
    val expectedAxisY = Array[BigDecimal](1.00, 1.05, 1.25, 1.45, 1.70, 2.00)

    val tableRead = binAdapter.tableRead2D("Boost target offset (sport)")

    tableRead.data.xAxis shouldBe expectedAxisX
    tableRead.data.yAxis shouldBe expectedAxisY
  }

  it should "work for Max. torque at crank (LSB floating point)" in {
    binAdapter.tableRead("Max. torque at crank").head shouldBe BigDecimal("750.00")
  }

  it should "work for timing" in {
    val initialVals = binAdapter.tableRead2D("Timing (main)").data.values.take(5)

    initialVals shouldBe Array[BigDecimal](11.0, 13.5, 17.5, 23.0, 28.5)
  }

  it should "work for VMAX switch - set to FF" in {
    val tableRead = binAdapter.tableRead("VMAX switch - set to FF")
    tableRead.head shouldBe BigDecimal(0xff)
  }
  
  it should "work for Codeword component protection set to 02" in {
    val codeword = binAdapter.tableRead("Codeword component protection set to 02").head shouldBe BigDecimal(2)
  }
}
