package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import scala.io.Source

class XdfAddressCompareTest extends AnyFlatSpec {

  private val xdfFile  = Source.fromResource("00003076501103.xdf").mkString
  private val xdfModel = XdfParser.parse(xdfFile)

  "XdfAddressCompare" should "outer join table axis addresses by address" in {
    val rows = XdfAddressCompare.joinByAddress(xdfModel, xdfModel)
    val row  = rows.find(_.address == 0x6a1d8eL).getOrElse(fail("Expected row at address 0x6a1d8e"))

    row.matchType shouldBe "both"
    row.leftEntries should not be empty
    row.rightEntries should not be empty
  }

  it should "outer join table addresses by name and calculate address diff" in {
    val rows = XdfAddressCompare.joinByName(xdfModel, xdfModel)
    val row = rows
      .find(_.tableName == "Load to torque")
      .getOrElse(fail("Expected row for table 'Load to torque'"))

    row.leftAddress shouldBe Some(0x6a1abeL)
    row.rightAddress shouldBe Some(0x6a1abeL)
    row.addressDiff shouldBe Some(0L)
  }
}
