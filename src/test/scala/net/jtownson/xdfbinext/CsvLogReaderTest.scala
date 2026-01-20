package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import org.scalatest.matchers.should.Matchers._

class CsvLogReaderTest extends AnyFlatSpec {

  behavior of "CsvLogReader"

  it should "print some test data" in {
    (1 to 5).foreach { i =>
      (1 to 5).foreach { j =>
        println(s"$i $j ${i*i + j*j}")
      }
    }
  }

  it should "read a log" in {
    val log = new CsvLogReader(new File("src/test/resources/sample-log.csv"))

    val tExpected = List[BigDecimal](4121.993, 4122.264, 4122.458, 4122.618, 4122.843, 4123.067)

    log.channels("Time").take(6) shouldBe tExpected
    log.channels.values.foreach(channel => channel.length shouldBe 65)
  }
}
