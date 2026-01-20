package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.io.Source
import scala.math.BigDecimal.RoundingMode
import scala.util.Using

class JMTB58Test extends AnyFlatSpec {

  val xdfFile = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen1\\00003076501103.xdf"
  )
//  val bin1 = new File(
//    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\jmt\\jmt-daw-ff-comp-wip.bin"
//  )
  val bin1 = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs-original\\B58gen1\\00003076501103_original.bin"
  )

  private val xdfModel   = Using.resource(Source.fromFile(xdfFile))(r => XdfParser.parse(r.mkString))
  private val binAdapter = new XDFBinAdapter(bin1, xdfModel)

  it should "scale load to torque" in {

    val loadToTorque = binAdapter.tableRead2D("Load to torque")

    val scaled = loadToTorque.data.scaleX(BigDecimal(220) / BigDecimal(200))

    println(Data2Str.data2Str(scaled, 1))

  }

  it should "calculate the turbine massflow" in {

    def turbineMassflow(
        pwrTrb: BigDecimal,
        turbinePressureRatio: BigDecimal,
        exhaustGasTempC: BigDecimal
    ): BigDecimal = {
      val turbineEff_T = binAdapter.tableRead1D("BMWtchctr_fac_TrbEffIvs_T")
      val turbineExp_T = binAdapter.tableRead1D("BMWtchctr_fac_TrbExp_T")

      val turbineEff_uw = turbineEff_T.data.atX(turbinePressureRatio)
      val turbExp_uw    = turbineExp_T.data.atX(turbinePressureRatio)
      val cppExGas_uw   = BigDecimal(1000)

      val cppExGasK = exhaustGasTempC + 273.1

//      println(s"pwr_Trb_uw = $pwrTrb", )

      pwrTrb * turbineEff_uw * turbExp_uw * cppExGas_uw * 3.6 * 0.277778 / cppExGasK
    }

    val pwrTurb      = (30 to 30).map(BigDecimal(_))
    val pressRatTurb = (1 to 40 by 5).map(BigDecimal(_) / 10).filterNot(_ < 1)
    val exhT         = BigDecimal(550)

    println(s"pwrTurb,pressureRat,mfTurb")
    pwrTurb.foreach { pwr =>
      pressRatTurb.foreach { pressureRat =>
        val mfTurb = turbineMassflow(pwr, pressureRat, exhT)

        println(s"$pwr,$pressureRat,$mfTurb")
      }
    }
  }

  it should "linearly scale the compressor characteristic" in {
    val compressorCharacteristic =
      binAdapter.tableRead2D("Compressor characteristic with required compressor / turbine power").data

    /*
      f(0, 0)=0    A    f(3.5, 0)=0

      B                     C

      f(570,0)=0   D    f(3.5, 570)=70
     */
    // curve fA = 0
    // curve fB = 0
    // curve fC = 70 / 570 y
    // curve fD = 70 / 3.5 x

    // map x axis into range (1 to 3.5)
    val scaledX = (0 to 19).map(i => BigDecimal(1) + i * (3.5 - 1) / 19).toArray

    scaledX.map(_.setScale(3, RoundingMode.HALF_UP)).foreach(println)
    // likewise for y. map into (15, 570)
    val scaledY = (0 to 15).map(i => BigDecimal(15) + i * (570 - 15) / 15).toArray

    // map S58 values into range required by B58
    val scaledValues =
      scaledY.flatMap { y =>
        // get function value at left and right endpoints by reading lines B and C
        val fb = 0
        val fc = 70 * y / 570
//        println(s"$y, $fc")
        scaledX.map { x =>
          (fc - fb) * x / 3.5
        }
      }

    val scaledCompressor = Interpolated2D(scaledX, scaledY, scaledValues)

    println(Data2Str.data2Str(scaledCompressor, 3))
  }

  it should "print the load request at full throttle" in {
    val torqueRequestCeiling = binAdapter.tableRead1D("Torque request ceiling").data

    val loadToTorque = binAdapter.tableRead2D("Load to torque").data
    val torqueToLoad = loadToTorque.invertedX

    val rpms = torqueRequestCeiling.axis

    val loadReq = rpms.map { rpm =>
      val torque = torqueRequestCeiling.atX(rpm)
      torqueToLoad.atXY(torque, rpm)
    }

    println(Data2Str.data2Str(Interpolated1D(rpms, loadReq), 2))
  }

  it should "invert the torque table" in {

    val loadToTorque = binAdapter.tableRead2D("Load to torque").data

    val (xp, yp, zp) = Invert.tableInvertX(
      xAxis = loadToTorque.xAxis,
      yAxis = loadToTorque.yAxis,
      z = loadToTorque.values
    )

    val xps = xp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)
    val yps = yp.map(_.setScale(0, RoundingMode.HALF_UP)).map(_.toString)
    val zps = zp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)

    val rpms: Array[BigDecimal] =
      Array(800, 900, 1000, 1250, 1380, 2500, 3000, 3500, 4000, 4500, 4750, 5000, 5200, 5500, 6000, 6500, 6750, 7000)

    //    val torques =
//      rpmAxis
//        .map(rpm => loadToTorque.atXY(180, rpm).setScale(1, RoundingMode.HALF_UP))

//    println(torques.mkString("\n"))
//    println(Data2Str.data2Str1D(rpms.map(_.toString).toArray, torques.toArray))

    rpms.foreach { rpm =>
      println(s"$rpm, ${loadToTorque.atXY(BigDecimal(190), rpm)}")
    }
  }
}
