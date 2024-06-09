package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.{CharacteristicType, DataType}
import net.jtownson.xdfbinext.A2LBinAdapterTest.{a2LBinAdapter, a2LWrapper}
import net.jtownson.xdfbinext.a2l.CurveType.{NumberNumberTable1D, NumberStringTable1D, StringNumberTable1D}
import net.jtownson.xdfbinext.a2l.MapType.{NumberNumberNumberTable2D, NumberNumberStringTable2D, NumberStringNumberTable2D}
import net.jtownson.xdfbinext.a2l.StringArray
import net.jtownson.xdfbinext.a2l.ValBlkConsumer.ValBlkType
import net.jtownson.xdfbinext.a2l.ValueConsumer.ValueType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.HALF_UP

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
    a2LBinAdapter.readCharacteristic("BMWtchad_fac_IpOfs_C") shouldBe BigDecimal("0.00500")
  }

  it should "read a UWORD value" in {
    a2LBinAdapter.readCharacteristic("BMWtchctr_fac_FilLimPctl_C") shouldBe BigDecimal("0.07001")
  }

  it should "read a SWORD curve" in {
    val expectedValues = Array[BigDecimal](120.00, 120.00, 120.00, 70.00)
    val expectedAxis   = Array[BigDecimal](2000, 2500, 4000, 4200)

    a2LBinAdapter.readCharacteristic("BMWtchbas_pct_AccrFlGcMin_T") shouldBe NumberNumberTable1D(
      expectedAxis,
      expectedValues
    )
  }

  it should "read a UWORD curve" in {
    val expectedValues = Array[BigDecimal](6.000, 40.000, 100.000, 160.000, 280.000, 450.000, 550.000, 660.000)
    val expectedAxis   = Array[BigDecimal](38.094, 120.000, 270.000, 400.000, 600.000, 900.000, 1050.000, 1200.000)

    a2LBinAdapter.readCharacteristic("BMWausy_p_DifCat_T") shouldBe NumberNumberTable1D(expectedAxis, expectedValues)
  }

  it should "read a SWORD map" in {
    val expectedX      = Array[BigDecimal](1, 1000, 2000, 3000, 4000, 5000)
    val expectedY      = Array[BigDecimal](0.00, 10.00, 20.00, 30.00, 40.00, 50.00)
    val expectedValues = Array.fill[BigDecimal](36)(850.0)

    a2LBinAdapter
      .readCharacteristic("BMWtchctr_t_ExGasMdl_M") shouldBe NumberNumberNumberTable2D(
      expectedX,
      expectedY,
      expectedValues
    )
  }

  it should "read a UWORD map" in {
    // format: off
    val expectedX = Array[BigDecimal](1050.000, 1130.000, 1200.000, 1300.000, 1350.000, 1400.000, 1450.000, 1525.000)
    val expectedY = Array[BigDecimal](25.00, 30.00, 35.00, 40.00, 60.00, 80.00)
    val expectedValues = Array[BigDecimal](
      3.000,	3.000,	3.000,	2.874,	2.664,	2.329,	2.150,	2.150,
      3.000,	3.000,	3.000,	2.874,	2.664,	2.329,	2.150,	2.150,
      3.000,	3.000,	3.000,	2.874,	2.664,	2.329,	2.150,	2.150,
      3.000,	3.000,	3.000,	2.874,	2.664,	2.329,	2.150,	2.150,
      3.000,	3.000,	3.000,	2.874,	2.650,	2.198,	2.000,	2.000,
      3.000,	3.000,	3.000,	2.874,	2.650,	2.184,	2.000,	2.000)
    
    a2LBinAdapter.readCharacteristic("BMWtchsp_rat_p_CmprMax_M") shouldBe NumberNumberNumberTable2D(expectedX, expectedY, expectedValues)
    // format: on
  }

  it should "read curve with axis" in {
    val expectedAxis   = Array[BigDecimal](1.70001, 1.79999, 1.90002, 2.20001, 2.59998, 3.99994)
    val expectedValues = Array[BigDecimal](1.59998, 1.62000, 1.65002, 1.71997, 1.75000, 1.79999)
    val c =
      a2LBinAdapter.readCharacteristic("BMWtchctr_fac_TrbEffIvs_T") shouldBe NumberNumberTable1D(
        expectedAxis,
        expectedValues
      )
  }

  it should "read a val_blk" in {
    a2LBinAdapter.readCharacteristic("DINH_FId.DFC_Fan1SCB_CA") shouldBe StringArray(
      Array(
        "FID_BMW_TMK_DRLPLAUS",
        "FID_CKKOS",
        "FID_CMLE",
        "FId_Fan1CircErr",
        "FId_Fan1Off",
        "FId_Fan1PsErr",
        "FId_Fan1PwrStg",
        "FId_Fan1PwrStgReEnaDis",
        "FId_FanDiagFan1SCB",
        "FId_Unused",
        "FId_Unused",
        "FId_Unused"
      )
    )
  }

  it should "read a map with STD_AXIS" in {
    val expectedX = Array[BigDecimal](720.00, 1000.00, 1240.00, 1520.00, 2000.00, 2520.00, 3000.00, 3520.00, 4000.00,
      4520.00, 5000.00, 5520.00, 5800.00, 6000.00, 6520.00, 6960.00).map(_.setScale(2, HALF_UP))
    val expectedY      = Array[BigDecimal](45.0, 80.3, 130.5, 160.5).map(_.setScale(1, HALF_UP))
    val expectedValues = Array.fill[BigDecimal](expectedY.length * expectedX.length)(BigDecimal("25.50000"))

    a2LBinAdapter
      .readCharacteristic("IKCtl_facRefMxSctn2_M") shouldBe NumberNumberNumberTable2D(
      expectedX,
      expectedY,
      expectedValues
    )
  }

  it should "read a single boolean" in {
    a2LBinAdapter.readCharacteristic("B_VMDEAK397_V") shouldBe "false"
  }

  it should "read a float32 value" in {
    a2LBinAdapter.readCharacteristic("K_RFVISTWOUT_BIAS") shouldBe BigDecimal("-56.7423248")
  }

  it should "read a curve with vtab axis" in {
    a2LBinAdapter.readCharacteristic("CoEng_nrWpDmdReqIgrThd_CUR") shouldBe StringNumberTable1D(
      Array("No demand", "ExhCo"),
      Array(25.0, 25.0)
    )
  }

  it should "read a curve with vtab values" in {
    // format: off
    val expectedAxis = (0 to 19).map(n => BigDecimal(s"$n.000")).toArray // Array[BigDecimal](0.000, 1.000, 2.000, 3.000, 4.000, 5.000, 6.000, 7.000, 8.000, 9.000, 10.000, 11.000, 12.000, 13.000, 14.000, 15.000, 16.000, 17.000, 18.000, 19.000)
    val expectedValues = Array[String]("true", "true", "false", "true", "true", "true", "true", "true", "true", "true", "false", "false", "false", "false", "false", "false", "false", "false", "false", "false")
    a2LBinAdapter.readCharacteristic("KL_SPORT_MDGWF") shouldBe  NumberStringTable1D(expectedAxis, expectedValues)
    // format: on
  }

  it should "read a value using tab" in {
    a2LBinAdapter.readCharacteristic("c_ctr_resp_rep_comc_nvld") shouldBe BigDecimal(3)
  }

  it should "read a map using tab values" in {
    val expectedX    = (0 to 9).map(BigDecimal(_)).toArray
    val expectedY    = (0 to 9).map(n => BigDecimal(s"$n.000")).toArray
    val expectedVals = Array.fill(100)("false")

    a2LBinAdapter.readCharacteristic("KF_ANSCHLAGREG_NO_POS") shouldBe NumberNumberStringTable2D(
      expectedX,
      expectedY,
      expectedVals
    )
  }

  it should "read a map with string axes and numeric values" in {
    // format: off
    val expectedX = Array[BigDecimal](1, 2, 4, 6)
    val expectedY = Array[String]("HDR OR", "HD OR", "HD low", "HD krit")
    val expectedVals = Array[BigDecimal](
        0,  1,  0,  1,
        2,  3,  0,  2,
        3,  3,  0,  3,
        4,  4,  4,  4
    )

    a2LBinAdapter.readCharacteristic("BMWlpsd_st_PRailReq_M") shouldBe NumberStringNumberTable2D(expectedX, expectedY, expectedVals)
    // format: on
  }

  it should "read ip_cppwm_inc_nvld (which has axis tabs)" in {
    val expectedAxis   = Array[BigDecimal](0.000000, 30.078125, 50.000000, 80.078125, 99.609375)
    val expectedValues = Array[BigDecimal](0.000000, 10.156250, 19.921875, 30.078125, 39.843750)
    a2LBinAdapter.readCharacteristic("ip_cppwm_inc_nvld") shouldBe NumberNumberTable1D(expectedAxis, expectedValues)
  }

  it should "read KLTNMXPRATEXP" in {
    val expectedAxis   = (-1 to 9).map(n => BigDecimal(s"$n.000000")).toArray
    val expectedValues = Array.fill(11)(BigDecimal("0.000000"))
    a2LBinAdapter.readCharacteristic("KLTNMXPRATEXP") shouldBe NumberNumberTable1D(expectedAxis, expectedValues)
  }

  it should "read KFKRINTZ1GF" in {
    val expectedX      = (0 to 5).map(n => BigDecimal(s"$n.000")).toArray
    val expectedY      = (0 to 4).map(n => BigDecimal(s"$n.000")).toArray
    val expectedValues = Array.fill(30)(BigDecimal("1.000"))

    a2LBinAdapter
      .readCharacteristic("KFKRINTZ1GF") shouldBe NumberNumberNumberTable2D(expectedX, expectedY, expectedValues)
  }

  it should "read KL_TD_HBA_F" in {
    val expectedAxis   = Array("Eco", "Normal", "High", "MAX")
    val expectedValues = Array("5.000", "5.000", "10.000", "15.000").map(BigDecimal(_))

    a2LBinAdapter.readCharacteristic("KL_TD_HBA_F") shouldBe StringNumberTable1D(expectedAxis, expectedValues)
  }

  it should "read the software version" in {
    a2LBinAdapter.readCharacteristic("CustDiag_dDatasetVer_C") shouldBe "R1C9J8B3BCEDX2"
  }

  it should "read all characteristics in the a2l" in {
    a2LWrapper.characteristics.foreach { (n, c) =>
      a2LBinAdapter.readCharacteristic(n)
    }
  }
}

object A2LBinAdapterTest {
  private val originalBin = new File("src/test/resources/00003076501103_original.bin")

  private val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL

  private val a2LWrapper = A2LWrapper(a2lUrl)

  private val a2LBinAdapter = new A2LBinAdapter(originalBin, a2LWrapper)
}
