package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfDiffTest.XdfDiff
import net.jtownson.xdfbinext.XdfSchema.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source
import scala.util.Using

class XdfDiffTest extends AnyFlatSpec {

  private val f1 = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen2\\00005D55502807\\00005D55502807.xdf"
  )

  private val xdf1 = XdfParser.parse(Using.resource(Source.fromFile(f1))(_.mkString))

  private val f2 = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen2\\00005D553C9607\\00005D553C9607.xdf"
  )

  private val xdf2 = XdfParser.parse(Using.resource(Source.fromFile(f2))(_.mkString))

  private def toDiff(tleft: XdfTable, tright: XdfTable): XdfDiff = {
    val addrLeft  = tleft.axes.z.embeddedData.mmedAddress
    val addrRight = tright.axes.z.embeddedData.mmedAddress
    XdfDiff(tleft.title, addrLeft, addrRight)
  }

  "this" should "work" in {
    val diffs = xdf1.tables.flatMap { tleft =>
      val maybeTRight = xdf2.tablesByName.get(tleft.title)

      maybeTRight.map(tright => toDiff(tleft, tright)).toSeq
    }
    Using.resource(new PrintWriter(new FileOutputStream(s"00005D55502807-vs-00005D553C9607.csv"))) { out =>
      out.println("table,matches,addr-left,addr-right,offset")
      diffs.foreach { diff =>
        out.println(
          s"${diff.tableName},${diff.isAMatch},${diff.addressLeftHexStr},${diff.addressRightHexStr},${diff.offsetHexStr}"
        )
      }
    }
  }

}

object XdfDiffTest {
  case class XdfDiff(tableName: String, addressLeft: Long, addressRight: Long) {
    val isAMatch: Boolean          = addressLeft == addressRight
    val offset: Long               = addressRight - addressLeft
    val addressLeftHexStr: String  = s"0x${addressLeft.toHexString}"
    val addressRightHexStr: String = s"0x${addressRight.toHexString}"
    val offsetHexStr: String       = if (offset < 0) s"0x${(-1 * offset).toHexString}" else s"0x${offset.toHexString}"
  }
}
