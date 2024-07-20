package net.jtownson.xdfbinext;

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import scala.io.Source

class ReportExtractorTest extends AnyFlatSpec {

  "ReportExtractor" should "extract notes" in {
    val in        = text.split('\n')
    val extracted = ReportExtractor.notes(in)
    extracted("Torque request ceiling") shouldBe """  Not related to E85. Just dialling this down at low RPM."""
    extracted("Load Interpolation (Map 1)") shouldBe
      """   Try linear scaling from 0..2 for E0 to E80.
          |
          |   This is a second para""".stripMargin
  }

  it should "extract ordered table listing" ignore {
    val in     = text.split('\n')
    val tables = ReportExtractor.tables(in)
    tables shouldBe List(
      "Torque request ceiling",
      "Load Interpolation (Map 1)",
      "Load Interpolation (Map 2)"
    )
  }

  it should "not crash for old style report" ignore {
    val in = Source.fromResource("stage-1-vs-stage-2-hpfp.txt")
    noException shouldBe thrownBy(ReportExtractor.notes(in.getLines().to(Iterable)))
  }

  val text = """
               |=== Torque request ceiling ===
               |
               |'''Description''': BMWtqe_tqc_FlApplStgNorm_T
               |
               |'''Dimension''': 1D, vector
               |
               |'''Categories''': Limits
               |
               |'''Unit info''': 1/min --> Nm
               |
               |'''Breakpoints''': Torque request ceiling X (autogen)
               |
               |
               |'''Base''':
               |
               |   800   900  1000  1250  1380  2500  3000  3500  4000  4500  4750  5000  5200  5500  6000  6500  6750  7000
               | 685.0 685.0 685.0 685.0 750.0 750.0 750.0 750.0 850.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0
               |
               |'''Difference''':
               |
               |    800    900   1000   1250   1380   2500   3000   3500   4000   4500   4750   5000   5200   5500   6000   6500   6750   7000
               | -385.0 -385.0 -385.0 -335.0 -350.0 -175.0  -75.0    0.0  -35.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
               |
               |'''Modified''':
               |
               |   800   900  1000  1250  1380  2500  3000  3500  4000  4500  4750  5000  5200  5500  6000  6500  6750  7000
               | 300.0 300.0 300.0 350.0 400.0 575.0 675.0 750.0 815.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0 900.0
               |
               |'''Notes''':
               |
               |  Not related to E85. Just dialling this down at low RPM.
               |
               |=== Load Interpolation (Map 1) ===
               |
               |'''BMW Name''':
               |
               |'''Dimension''': 1D, vector
               |
               |'''Categories''': MHD+ Suite, FlexFuel, Blend Factors
               |
               |'''Unit info''': E% --> -
               |
               |'''Breakpoints''': Load Interpolation (Map 1) - Ethanol Content
               |
               |
               |'''Base''':
               |
               |    0   10   20   30   40   50   60   70   80   90
               | 0.00 0.25 0.50 0.75 1.00 1.00 1.00 1.00 1.00 1.00
               |
               |'''Difference''':
               |
               |    0   10   20   30   40   50   60   70   80   90
               | 0.00 0.00 0.00 0.00 0.00 0.25 0.50 0.75 1.00 1.00
               |
               |'''Modified''':
               |
               |    0   10   20   30   40   50   60   70   80   90
               | 0.00 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00 2.00
               |
               |'''Notes''':
               |   Try linear scaling from 0..2 for E0 to E80.
               |
               |   This is a second para
               |
               |=== Load Interpolation (Map 2) ===
               |""".stripMargin

}
