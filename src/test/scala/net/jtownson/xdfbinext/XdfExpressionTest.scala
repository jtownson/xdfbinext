package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.InverseLookup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import scala.io.Source

class XdfExpressionTest extends AnyFlatSpec {

  private val xdfFile = Source.fromResource("00003076501103-jmt-expr.xdf.xml").mkString
  private val xdf     = XdfParser.parse(xdfFile)

  private val originalBin = new File(
    getClass.getClassLoader.getResource("00003076501103_original.bin").toURI
  )

  private val binAdapter = new BinAdapter(originalBin, xdf)

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
    xdf.virtualTablesByName("Torque to load").tableDef shouldBe InverseLookup("Load to torque", "x")
  }

  it should "read an inverse table" in {
    // here we want to be able to feed in values to perform a table lookup
    // and perform the same for a virtual table
    /*
    t = t(load, rpm)
          0.0	12.0	14.0
        --------------------
    500 | 0.0 	29.1	34.0
    600 | 0.0 	32.6	38.0
    800 | 0.0 	36.0	42.0
    1000| 0.0 	36.9	43.0


    we want a function g(t, rpm) = load
    i.e. in the first case we know load and rpm
    in the second we know rpm and torque
    so to get the inverse, we note the range of torque values is in [0.0, 43.0]
    which gives us the torque axis range
    l = t' = l(torque, rpm)
           0    43/3    2 * 43/3
    500 |  0
    600 |  0
    800 |  0
    1000|  0
     */
//    binAdapter.tableRead("Torque to load")
  }
}
