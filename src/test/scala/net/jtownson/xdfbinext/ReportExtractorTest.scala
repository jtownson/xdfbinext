package net.jtownson.xdfbinext;

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class ReportExtractorTest extends AnyFlatSpec {

  "ReportExtractor" should "extract notes" in {
    val in        = text.split('\n')
    val extracted = ReportExtractor.notes(in)
    extracted shouldBe Map(
      "Normierung für Mdk_ist_sot_mem_mx" -> """    A normalizing factor that looks like a clutch torque limit, upped to 550Nm in the stage 1 map.""",
      "Max Torque at clutch (monitoring)" -> """   Again, max clutch torque upped to 550Nm."""
    )
  }

  it should "extract ordered table listing" in {
    val in     = text.split('\n')
    val tables = ReportExtractor.tables(in)
    tables shouldBe List(
      "Normierung für Mdk_ist_sot_mem_mx",
      "Max Torque at clutch (monitoring)",
      "Max power (monitoring)"
    )
  }

  val text = """
               |
               |Table (scalar): Normierung für Mdk_ist_sot_mem_mx
               |Unit info: -
               |Categories: Limits
               |Base:
               |  500.000
               |
               |Difference:
               |   50.000
               |
               |Modified:
               |  550.000
               |
               |Notes:
               |    A normalizing factor that looks like a clutch torque limit, upped to 550Nm in the stage 1 map.
               |
               |
               |Table (scalar): Max Torque at clutch (monitoring)
               |Unit info: Nm
               |Categories: Limits
               |Base:
               |  500.000
               |
               |Difference:
               |   50.000
               |
               |Modified:
               |  550.000
               |
               |Notes:
               |   Again, max clutch torque upped to 550Nm.
               |
               |
               |Table (scalar): Max power (monitoring)
               |""".stripMargin

}
