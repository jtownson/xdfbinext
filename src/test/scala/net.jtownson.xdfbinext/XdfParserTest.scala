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

  it should "parse description" in {
    xdfModel.xdfHeader.description shouldBe "this is the description"
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
    xdfModel.xdfHeader.categories(0) shouldBe Category(index = 0, name = "Boost")
    xdfModel.xdfHeader.categories(1) shouldBe Category(index = 1, name = "Boost breakpoints")
    xdfModel.xdfHeader.categories(71) shouldBe Category(index = 0x47, name = "ReFlex Integration")
  }

  it should "parse table header" in {
    xdfModel.tables(0).uniqueId shouldBe 0x0
    xdfModel.tables(0).flags shouldBe 0x0
    xdfModel.tables(0).title shouldBe "Fan PWM based on coolant act to target delta X (autogen)"
  }

  it should "parse table category mem" in {
    // <CATEGORYMEM index="0" category="33"/>
    //      <CATEGORYMEM index="1" category="34"/>
    xdfModel.tables(0).categoryMems(0) shouldBe CategoryMem(index = 0, category = 33)
    xdfModel.tables(0).categoryMems(1) shouldBe CategoryMem(index = 1, category = 34)
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
      math = Math(equation = "X", vars = Seq(Var(id = "X")))
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
      math = Math(equation = "X", vars = Seq(Var(id = "X")))
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
      math = Math(equation = "X/100", vars = Seq(Var(id = "X")))
    )
  }

  it should ""
