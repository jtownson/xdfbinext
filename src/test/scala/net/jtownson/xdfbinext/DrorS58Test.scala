package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2LWrapperTest.a2lUrl
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.io.Source
import scala.math.BigDecimal.RoundingMode
import scala.util.Using
import scala.jdk.CollectionConverters.*
import org.scalatest.matchers.should.Matchers._

class DrorS58Test extends AnyFlatSpec {

  val xdfFile = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\Dror-Itzhari\\00005C64146E06.xdf"
  )
  val bin1 = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\Dror-Itzhari\\00005C64146E06_original.bin"
  )

  private val xdfModel   = Using.resource(Source.fromFile(xdfFile))(r => XdfParser.parse(r.mkString))
  private val binAdapter = new XDFBinAdapter(bin1, xdfModel)

  it should "calculate the PI duty cycles for E50" in {
    val dmeBin = new File(
      "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\Dror-Itzhari\\dror-s58-stg12-e40-PI-v2.bin"
    )

    val motivLiteBin = new File(
      "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\Dror-Itzhari\\V4.00_Motiv_ReFlex Lite_750cc_dror_PI_v1.bin"
    )

    val motivLiteXDFFile = new File(
      "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\tuning-repo\\Dror-Itzhari\\V4.01_ReFlex Lite_XDF.xdf"
    )

    val motivLiteXDF = Using.resource(Source.fromFile(motivLiteXDFFile))(r => XdfParser.parse(r.mkString))

    val dmeBinAdapter       = new XDFBinAdapter(dmeBin, xdfModel)
    val motivLiteBinAdapter = new XDFBinAdapter(motivLiteBin, motivLiteXDF)

    val ev14750CCE50FlowRateGms = BigDecimal("0.009351827")
    val e50StoichMixture        = BigDecimal("11.9")
    val fuelTable               = dmeBinAdapter.tableRead2D("Fuel (Bank 1)").data
    val loadAxis                = fuelTable.yAxis
    val rpmAxis                 = fuelTable.xAxis

    val tot = fuelTable.map { (rpm, load, fuelTableValue) =>
      val afr = e50StoichMixture * fuelTableValue / 14.7
      (rpm, load, totalFuelMassG(afr, load, 500))
    }

    val pi1 = fuelTable.map { (rpm, load, fuelTableValue) =>
      val afr = e50StoichMixture * fuelTableValue / 14.7
      (rpm, load, injectorPulseWidthMillis(afr, load, 500, 0.125, ev14750CCE50FlowRateGms))
    }

    println(Data2Str.data2Str(pi1, 3))
  }

  private def piFuelMass(
      afr: BigDecimal,
      loadPct: BigDecimal,
      cylVolCC: BigDecimal,
      knownDiFuelMassG: BigDecimal
  ): BigDecimal = {
    val airMassDensityGCC = BigDecimal("0.001293")
    val cylAirMassAtmos   = airMassDensityGCC * cylVolCC

    val totalFuelMass = (1 / (afr + 1)) * (loadPct / 100) * cylAirMassAtmos
    val piFuelMass    = totalFuelMass - knownDiFuelMassG
    if (piFuelMass < 0) 0 else piFuelMass
  }

  private def piFuelMassFraction(
      afr: BigDecimal,
      loadPct: BigDecimal,
      cylVolCC: BigDecimal,
      knownDiFuelMassG: BigDecimal
  ): BigDecimal = {
    val totalFuelMass              = totalFuelMassG(afr, loadPct, cylVolCC)
    val piFuelMass                 = totalFuelMass - knownDiFuelMassG
    val piFuelMassNorm: BigDecimal = if (piFuelMass < 0) 0 else piFuelMass
    piFuelMassNorm / totalFuelMass
  }

  private def totalFuelMassG(afr: BigDecimal, loadPct: BigDecimal, cylVolCC: BigDecimal): BigDecimal = {
    val airMassDensityGCC = BigDecimal("0.001293")
    val cylAirMassAtmos   = airMassDensityGCC * cylVolCC
    (1 / (afr + 1)) * (loadPct / 100) * cylAirMassAtmos
  }

//  private def fuelVolFlowCCMin(fuelMassG: BigDecimal, rpm: BigDecimal, fuelMassDensityGCC: BigDecimal): BigDecimal = {}

  private def injectorPulseWidthMillis(
      afr: BigDecimal,
      loadPct: BigDecimal,
      cylVolCC: BigDecimal,
      knownDiFuelMassG: BigDecimal,
      injectorFlowGms: BigDecimal
  ): BigDecimal = {
    piFuelMass(afr, loadPct, cylVolCC, knownDiFuelMassG) / injectorFlowGms
  }

  it should "scale the compressor characteristic using S58 model" in {
    val compressorCharacteristic =
      binAdapter.tableRead2D("Compressor characteristic with required compressor / turbine power").data

    val minX2 = BigDecimal("1.0")
    val maxX2 = BigDecimal("3.5")
    val minY2 = BigDecimal("15")
    val maxY2 = BigDecimal("570")

    // map S58 values into range required by B58
    val max          = compressorCharacteristic.values.max
    val fac          = BigDecimal("70.145") / max
    val scaledValues = compressorCharacteristic.values.map(_ * fac)

    // map S58 x axis (1 to 3.6) into range required by b58 (1 to 3.5)
    val scaledX = {
      val minX1 = compressorCharacteristic.xAxis.min
      val maxX1 = compressorCharacteristic.xAxis.max
      val mX    = (maxX2 - minX2) / (maxX1 - minX1)
      val cX    = minX2 - (maxX2 - minX2) * minX1 / (maxX1 - minX1)
      compressorCharacteristic.xAxis.map { x =>
        mX * x + cX
      }
    }

    // likewise for y. Map (14, 666) to (15, 570)
    val scaledY = {
      val minY1 = compressorCharacteristic.yAxis.min
      val maxY1 = compressorCharacteristic.yAxis.max
      val mY    = (maxY2 - minY2) / (maxY1 - minY1)
      val cY    = minY2 - (maxY2 - minY2) * minY1 / (maxY1 - minY1)
      compressorCharacteristic.yAxis.map { y =>
        mY * y + cY
      }
    }

    val scaledCompressor = Interpolated2D(scaledX, scaledY, scaledValues)

    val x = (0 to 19).map { i =>
      minX2 + i * (maxX2 - minX2) / 19
    }.toArray

    val y = (0 to 15).map { i =>
      minY2 + i * (maxY2 - minY2) / 15
    }.toArray

    val compressorNew = for {
      yi <- y
      xi <- x
    } yield scaledCompressor.atXY(xi, yi)

    val compFinal = Interpolated2D(x, y, compressorNew)

    println(Data2Str.data2Str(compFinal, 3))
  }

  it should "scale load to torque" in {

    val loadToTorque = binAdapter.tableRead2D("Load to torque")

    val scaled = loadToTorque.data.scaleX(1.071)

    println(Data2Str.data2Str(scaled, 1))
//    val (xp, yp, zp) = Invert.tableInvertX(
//      xAxis = loadToTorque.data.xAxis,
//      yAxis = loadToTorque.data.yAxis,
//      z = loadToTorque.data.values
//    )
//
//    val xps = xp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)
//    val yps = yp.map(_.setScale(0, RoundingMode.HALF_UP)).map(_.toString)
//    val zps = zp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)
//
//    val rpms =
//      Seq[BigDecimal](1000, 2000, 2500, 2700, 3000, 3500, 4000, 4500, 5000, 5509, 5545, 5750, 5950, 6250, 6500, 6750,
//        7000, 7300)
//
//    val torques =
//      rpms
//        .map(rpm => loadToTorque.data.atXY(240, rpm).setScale(1, RoundingMode.HALF_UP))
//
//    println(Data2Str.data2Str1D(rpms.map(_.toString).toArray, torques.toArray))

  }

  it should "invert the torque table" in {

    val loadToTorque = binAdapter.tableRead2D("Load to torque")

    val (xp, yp, zp) = Invert.tableInvertX(
      xAxis = loadToTorque.data.xAxis,
      yAxis = loadToTorque.data.yAxis,
      z = loadToTorque.data.values
    )

    val xps = xp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)
    val yps = yp.map(_.setScale(0, RoundingMode.HALF_UP)).map(_.toString)
    val zps = zp.map(_.setScale(1, RoundingMode.HALF_UP)).map(_.toString)

    val rpms =
      Seq[BigDecimal](1000, 2000, 2500, 2700, 3000, 3500, 4000, 4500, 5000, 5509, 5545, 5750, 5950, 6250, 6500, 6750,
        7000, 7300)

    val torques =
      rpms
        .map(rpm => loadToTorque.data.atXY(240, rpm).setScale(1, RoundingMode.HALF_UP))

    println(Data2Str.data2Str1D(rpms.map(_.toString).toArray, torques.toArray))
//    println(Data2Str.data2Str2D(xps, yps, zps))
  }

  it should "work out the rf limit thing" in {
//    val a2l = Using.resource(Source.fromResource("DME86S0_F4C9G576B.a2l").getU)
    val a2lFile    = new File("src/test/resources/DME86S0_F4C9G576B.a2l").toURI.toURL
    val a2lWrapper = A2LWrapper(a2lFile)
    a2lWrapper.compuVTabs.foreach { (name, vt) =>
      val pairs = vt.getValuePairs.asScala.toList.map(vp => vp.getInVal.toInt -> vp.getOutVal)

      val itsValues = pairs.map(_._1).sorted

      if (itsValues.startsWith(List(0, 1, 2, 3, 4, 5, 6, 7, 8)) && itsValues.length < 20) {
        println(vt)
        println()
        println()
      }
    }
  }
}
