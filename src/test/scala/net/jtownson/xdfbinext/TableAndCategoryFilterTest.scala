package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.TableAndCategoryFilter.filterTables
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.io.Source

class TableAndCategoryFilterTest extends AnyFlatSpec {

  private val xdfModel = XdfParser.parse(Source.fromResource("00003076501103.xdf").mkString)

  "TableAndCategoryFilter" should "not exclude tables when no filter" in {
    filterTables(xdfModel.tables, Set.empty, Set.empty) shouldBe xdfModel.tables
  }

  it should "exclude tables matching a table filter" in {
    val filtered = filterTables(
      xdfModel.tables,
      Set("(Custom)", "(Antilag)", "(Map 2)", "(Map 3)", "(Map 4)", "(FF)", "(FF#2))"),
      Set.empty
    )

    val excluded = xdfModel.tablesByName("Lambda Limit (Rich) (Map 2)")
    filtered.contains(excluded) shouldBe false
  }

  it should "exclude tables with a given category" in {
    val filtered = filterTables(
      xdfModel.tables,
      Set.empty,
      Set("Limits", "Boost", "Vanos")
    )

    val excluded = xdfModel.tablesByName("Max calculated power")

    filtered.contains(excluded) shouldBe false
  }

  "MapCompare" should "split args correctly" in {
    XDFMapCompare.csvReader.reads.apply("Limits, Boost, Vanos") shouldBe Set("Limits", "Boost", "Vanos")
  }
}
