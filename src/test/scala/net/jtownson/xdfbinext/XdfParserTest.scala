package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import scala.io.Source

class XdfParserTest extends AnyFlatSpec:

  private val xdfFile  = Source.fromResource("00003076501103.xdf").mkString
  private val xdfModel = XdfParser.parse(xdfFile)

  "XdfModel" should "have a version string" in {
    xdfModel.version shouldBe "1.70"
  }

  it should "parse flags" in {
    xdfModel.xdfHeader.flags shouldBe 0x1
  }

  it should "parse model description" in {
    xdfModel.xdfHeader.description shouldBe "this is the description"
  }

  it should "parse table descriptions" in {
    xdfModel.tablesByName("Wastegate Position Model (FF)").description shouldBe "BMWtchctr_pct_WgBasc_M (FF)"
  }

  it should "parse base offset" in {
    xdfModel.xdfHeader.baseOffset shouldBe BaseOffset(offset = 0, subtract = 0)
  }

  it should "parse defaults" in {
    // <DEFAULTS datasizeinbits="16" sigdigits="1" outputtype="1" signed="0" lsbfirst="0" float="0" />
    xdfModel.xdfHeader.defaults shouldBe Defaults(
      dataSizeInBits = 16,
      sigDigits = 1,
      outputType = 1,
      signed = 0,
      lsbfirst = 0,
      float = 0
    )
  }

  it should "parse region" in {
    // <REGION type="0xFFFFFFFF" startaddress="0x0" size="0x800000" regioncolor="0x0" regionflags="0x0" name="Binary File" desc="This region describes the bin file edited by this XDF" />
    xdfModel.xdfHeader.region shouldBe Region(
      `type` = 0xffffffff,
      startAddress = 0x0,
      size = 0x800000,
      regionColor = 0x0,
      regionFlags = 0x0,
      name = "Binary File",
      desc = "This region describes the bin file edited by this XDF"
    )
  }

  it should "parse categories" in {
    // <CATEGORY index="0x0" name="Boost" />
    // <CATEGORY index="0x1" name="Boost breakpoints" />
    // <CATEGORY index="0x47" name="ReFlex Integration" />
    xdfModel.xdfHeader.categories(0) shouldBe Category(index = "0x0", name = "Boost")
    xdfModel.xdfHeader.categories(1) shouldBe Category(index = "0x1", name = "Boost breakpoints")
    xdfModel.xdfHeader.categories(71) shouldBe Category(index = "0x47", name = "ReFlex Integration")
  }

  it should "add category titles to tables" in {
    /*
    <CATEGORYMEM index="0" category="39" />
    <CATEGORYMEM index="1" category="46" />
    <CATEGORYMEM index="2" category="3" />
     */
    xdfModel.tablesByName("Wastegate Position Model (FF)").categoryMems.map(_.category.name) shouldBe Seq(
      "MHD+ Suite",
      "FlexFuel",
      "WGDC"
    )
  }

  it should "parse table header" in {
    xdfModel.tables(0).uniqueId shouldBe 0x0
    xdfModel.tables(0).flags shouldBe 0x0
    xdfModel.tables(0).title shouldBe "Fan PWM based on coolant act to target delta X (autogen)"
  }

  it should "parse table category mem" in {
    // <CATEGORYMEM index="0" category="33"/>
    // <CATEGORYMEM index="1" category="34"/>
    xdfModel.tables(0).categoryMems(0) shouldBe CategoryMem(index = 0, category = Category("0x20", "Cooling"))
    xdfModel.tables(0).categoryMems(1) shouldBe CategoryMem(
      index = 1,
      category = Category("0x21", "Cooling breakpoints")
    )
  }

  it should "parse axes" in {
    xdfModel.tables(0).axes.x shouldBe XdfAxisX(
      id = "x",
      uniqueId = 0,
      embeddedData = EmbeddedData(
        mmedTypeFlags = 0,
        mmedAddress = -1L,
        mmedElementSizeBits = 16,
        mmedRowCount = 0,
        mmedColCount = 0,
        mmedMajorStrideBits = -32,
        mmedMinorStrideBits = 0
      ),
      indexCount = 12,
      dataType = 0,
      unitType = 0,
      daLink = DaLink(0),
      labels = Seq(
        Label(index = 0, value = "0.00"),
        Label(index = 1, value = "0.00"),
        Label(index = 2, value = "0.00"),
        Label(index = 3, value = "0.00"),
        Label(index = 4, value = "0.00"),
        Label(index = 5, value = "0.00"),
        Label(index = 6, value = "0.00"),
        Label(index = 7, value = "0.00"),
        Label(index = 8, value = "0.00"),
        Label(index = 9, value = "0.00"),
        Label(index = 10, value = "0.00"),
        Label(index = 11, value = "0.00")
      ),
      math = Math(equation = "X", vars = Seq(Var(id = "X"))),
      units = ""
    )

    xdfModel.tables(0).axes.y shouldBe XdfAxisY(
      id = "y",
      uniqueId = 0,
      embeddedData = EmbeddedData(
        mmedTypeFlags = 0,
        mmedAddress = -1L,
        mmedElementSizeBits = 16,
        mmedRowCount = 0,
        mmedColCount = 0,
        mmedMajorStrideBits = -32,
        mmedMinorStrideBits = 0
      ),
      indexCount = 1,
      dataType = 0,
      unitType = 0,
      daLink = DaLink(0),
      labels = Seq(Label(index = 0, value = "0.00")),
      math = Math(equation = "X", vars = Seq(Var(id = "X"))),
      units = ""
    )

    xdfModel.tables(0).axes.z shouldBe XdfAxisZ(
      id = "z",
      embeddedData = EmbeddedData(
        mmedTypeFlags = 1,
        mmedAddress = 0x6851b6,
        mmedElementSizeBits = 16,
        mmedRowCount = 1,
        mmedColCount = 12,
        mmedMajorStrideBits = 0,
        mmedMinorStrideBits = 0
      ),
      decimalPl = 1,
      min = BigDecimal("0.000000"),
      max = BigDecimal("255.000000"),
      outputType = 1,
      math = Math(equation = "X/100", vars = Seq(Var(id = "X"))),
      units = ""
    )
  }

  it should "parse x axis units" in {
    xdfModel.tablesByName("Intake Waterpump flow ( IAT minus ambient temp, ambient temp)").axes.x.units shouldBe "°C"
  }

  it should "parse y axis units" in {
    xdfModel.tablesByName("Intake Waterpump flow ( IAT minus ambient temp, ambient temp)").axes.y.units shouldBe "°C"
  }

  it should "parse z axis units" in {
    xdfModel.tablesByName("Intake Waterpump flow ( IAT minus ambient temp, ambient temp)").axes.z.units shouldBe "l/h"
  }

  it should "parse memaddress for axes with breakpoints" in {
    val table = xdfModel.tablesByName("Load to torque")
    table.axes.x.embeddedData.mmedAddress shouldBe 0x6a1d8e
    table.axes.y.embeddedData.mmedAddress shouldBe 0x6a1db2
  }

  it should "query constants" in {
    val tableName     = "Max calculated power"
    val expectedTable = xdfModel.tablesByName(tableName)

    xdfModel.tablesConstant(tableName) shouldBe expectedTable
  }

  it should "query 1D tables and associated breakpoints" in {
    val tableName     = "Torque Reduction Factor (RPM)"
    val xAxisName     = "Torque Reduction Factor (RPM) X (autogen)"
    val expectedTable = Table1DEnriched(xdfModel.tablesByName(tableName), xdfModel.tablesByName.get(xAxisName))

    xdfModel.tables1D(tableName) shouldBe expectedTable
  }

  it should "query 2D tables and associated breakpoints" in {
    val tableName = "Load to torque"
    val xAxisName = "Load to torque X (autogen)"
    val yAxisName = "Load to torque Y (autogen)"
    val expectedTable = Table2DEnriched(
      xdfModel.tablesByName(tableName),
      xdfModel.tablesByName.get(xAxisName),
      xdfModel.tablesByName.get(yAxisName)
    )

    xdfModel.tables2D(tableName) shouldBe expectedTable
  }

  it should "identity timing table as 1D" in {
    val table = "Timing (Main) (FF#2) - RPM"
    xdfModel.tables1D.get(table) should matchPattern { case Some(_) => }
  }

  it should "categorise MHD - codes list to deactivate" in {
    val tableName   = "MHD - codes list to deactivate"
    val t: XdfTable = xdfModel.tablesByName(tableName)
    val categorised = xdfModel.tables1D.contains(tableName)
    categorised shouldBe true
  }

  it should "categorise all tables as const, 1D or 2D" in {
    xdfModel.tablesByName.keySet.foreach { tableName =>
      val categorised = xdfModel.tablesConstant.contains(tableName) ||
        xdfModel.tables1D.contains(tableName) ||
        xdfModel.tables2D.contains(tableName)
      withClue(tableName) { categorised shouldBe true }
    }
  }

  it should "return true for isBreakpoint when a table is a breakpoint table" in {
    xdfModel.isBreakpointTable(xdfModel.tablesByName("Basic catalyst heating mode wish X (autogen)")) shouldBe true
    xdfModel.isBreakpointTable(xdfModel.tablesByName("Basic catalyst heating mode wish")) shouldBe false
  }
