package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.{CharacteristicType, DataType}
import net.jtownson.xdfbinext.A2LBinAdapterTest.{a2LBinAdapter, a2LWrapper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File

class A2LBinAdapterTest extends AnyFlatSpec {
  behavior of "A2LBinAdapter"

  def findChars(dataType: DataType, characteristicType: CharacteristicType): Unit = {
    a2LWrapper.characteristics.foreach { (n, c) =>
      if (a2LWrapper.getType(c) == dataType && c.getType == characteristicType && n.startsWith("BMWtch")) {
        println(n)
      }
    }
  }

  it should "read a SWORD value" in {
    a2LBinAdapter.readCharacteristic("BMWtchad_fac_IpOfs_C").head shouldBe BigDecimal("0.00500")
  }

  it should "read a UWORD value" in {
    a2LBinAdapter.readCharacteristic("BMWtchctr_fac_FilLimPctl_C").head shouldBe BigDecimal("0.07001")
  }

  it should "read a SWORD curve" in {
    a2LBinAdapter.readCharacteristic("BMWtchbas_pct_AccrFlGcMin_T").toSeq shouldBe Seq[BigDecimal](
      120.00,
      120.00,
      120.00,
      70.00
    )
  }

  it should "read a UWORD curve" in {
    a2LBinAdapter.readCharacteristic("BMWausy_p_DifCat_T").toSeq shouldBe Seq[BigDecimal](6.000, 40.000, 100.000,
      160.000, 280.000, 450.000, 550.000, 660.000)
  }

  it should "read a SWORD map" in {
    a2LBinAdapter.readCharacteristic("BMWtchctr_t_ExGasMdl_M").toSeq should contain only BigDecimal(850)
  }

  it should "read a UWORD map" in {
    a2LBinAdapter.readCharacteristic("BMWtchsp_rat_p_CmprMax_M").toSeq.take(16) shouldBe Seq[BigDecimal](3.000, 3.000,
      3.000, 2.874, 2.664, 2.329, 2.150, 2.150, 3.000, 3.000, 3.000, 2.874, 2.664, 2.329, 2.150, 2.150)
  }

  it should "read an axis" in {
    a2LBinAdapter.readAxis("BMWtchsp_rat_p_CmprMax_Ax").toSeq shouldBe Seq[BigDecimal](1050.000, 1130.000, 1200.000,
      1300.000, 1350.000, 1400.000, 1450.000, 1525.000)
  }

  it should "read map with axes" in {
    val m = a2LBinAdapter.readMap("BMWtchsp_rat_p_CmprMax_M")
    m.atXY(1050, 25) shouldBe BigDecimal("3.000")
    m.atXY(1525, 80) shouldBe BigDecimal("2.000")
  }

  it should "read curve with axis" in {
    val c = a2LBinAdapter.readCurve("BMWtchctr_fac_TrbEffIvs_T")
    c.values.toSeq shouldBe Seq[BigDecimal](1.59998, 1.62000, 1.65002, 1.71997, 1.75000, 1.79999)
    c.axis.toSeq shouldBe Seq[BigDecimal](1.70001, 1.79999, 1.90002, 2.20001, 2.59998, 3.99994)
  }

  it should "read a val_blk" in {
    pending
//    a2LBinAdapter.readCharacteristic("DINH_FId.DFC_Fan1SCB_CA").toSeq shouldBe ???
  }

  it should "read all characteristics in the a2l" ignore {
    a2LWrapper.characteristics.foreach { (n, c) =>
      if (
        c.getType == CharacteristicType.VALUE ||
        c.getType == CharacteristicType.CURVE ||
        c.getType == CharacteristicType.MAP
      ) {
        a2LBinAdapter.readCharacteristic(n) shouldBe an[Array[BigDecimal]]
      }
    }
  }
}

object A2LBinAdapterTest {
  private val originalBin = new File("src/test/resources/00003076501103_original.bin")

  val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL

  val a2LWrapper = A2LWrapper(a2lUrl)

  val a2LBinAdapter = new A2LBinAdapter(originalBin, a2LWrapper)
}
