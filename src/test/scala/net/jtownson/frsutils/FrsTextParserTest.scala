package net.jtownson.frsutils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import fastparse.*
import net.jtownson.frsutils.FrsTextParser.{
  Document,
  Linebreak,
  LinkingSectionHeading,
  PageNumber,
  SectionHeading,
  StandaloneHeading,
  pageNumber,
  parseDocument
}

class FrsTextParserTest extends AnyFlatSpec {

  "FrsTextParser" should "parse document title" in {
    val gvn = """I: First Project for N74TUE | MG1CS003_N74TUE C0C2J6E5B;0 | 24.11.2017"""
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(List(SectionHeading("I:", "First Project for N74TUE | MG1CS003_N74TUE C0C2J6E5B;0 | 24.11.2017"))),
      70
    )
  }

  it should "parse a page number" in {
    val gvn = "2 | 22242"
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(List(PageNumber("2", "22242"))),
      9
    )
  }

  it should "parse a numeric section header - type 1" in {
    val gvn = "1 Einführung, Hintergrundinformation"
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(List(SectionHeading("1", "Einführung, Hintergrundinformation"))),
      36
    )
  }

  it should "parse a numeric section header - type 2" in {
    val gvn = """2.1 Grafischer Linkmechanismus "ClickPic""""
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(List(SectionHeading("2.1", """Grafischer Linkmechanismus "ClickPic""""))),
      41
    )
  }

  it should "parse a standalone section title" in {
    val gvn = """Inhaltsverzeichnis"""
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(List(StandaloneHeading("Inhaltsverzeichnis"))),
      19
    )
  }

  it should "parse whitespace lines" in {
    val gvn = """
                |18 | 22242
                |""".stripMargin
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(
        List(
          Linebreak(),
          PageNumber("18", "22242"),
          Linebreak()
        )
      ),
      12
    )
  }

  it should "parse a simple contents entry " in {
    val gvn = "15.1 [CEL_Compu 1.153.0;0] DGS Umrechnungsformeln . . . . . . . . . . . . . . . . . . . . . . . . . 920"
    parseDocument(gvn) shouldBe Parsed.Success(
      Document(
        List(
          LinkingSectionHeading(
            "15.1",
            "CEL_Compu 1.153.0;0",
            "DGS Umrechnungsformeln . . . . . . . . . . . . . . . . . . . . . . . . . 920"
          )
        )
      ),
      103
    )
  }

  it should "parse a nasty contents entry" in {
    val gvn =
      "[CEL 3.164.0;0] Zentrale Elemente . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 920"

  }

}
