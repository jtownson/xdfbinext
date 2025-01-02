package net.jtownson.xdfbinext

import net.alenzen.a2l.{Measurement, MemorySegment}
import net.jtownson.xdfbinext.Measurement2UserChannel.{actualValueTemplate, actualValuesTemplate}
import net.jtownson.xdfbinext.a2l.{ByteBlock, FormulaExpressionInverse, RatFun}

import java.net.URL

class Measurement2UserChannel(a2lUrl: URL) {

  val a2l: A2LWrapper = A2LWrapper(a2lUrl)

  private val measurements = a2l.measurements
  private val compuMethods = a2l.compuMethods

  def measurement2UserChannel(namePredicate: String => Boolean): Seq[String] = {
    val applicableMeasurements = measurements.filter((n, _) => namePredicate(n)).values
    applicableMeasurements.flatMap(measurement2UserChannel).toSeq
  }

  def measurement2UserChannels(namePredicate: String => Boolean): String = {
    val applicableMeasurements = measurements.filter((n, _) => namePredicate(n)).values
    actualValuesTemplate(measurement2UserChannel(namePredicate))
  }

  private def measurement2UserChannel(m: Measurement): Seq[String] = {
    Option(m.getArraySize).fold(Seq(measurement2UserChannel(m, m.getEcuAddress, m.getName)))(size =>
      measurement2UserChannelArr(m, m.getEcuAddress, m.getName, size.toInt)
    )
  }

  private def measurement2UserChannelArr(m: Measurement, address: Long, name: String, size: Int): Seq[String] = {
    (1 to size).map { offset =>
      val sizeBytes = ByteBlock.sizeOf(m.getDatatype)
      measurement2UserChannel(m, address + offset * sizeBytes, s"${name}_$offset")
    }
  }

  private def measurement2UserChannel(m: Measurement, address: Long, name: String): String = {
    val segmentPrefix = a2l.segmentForAddress(address).toHexString.take(2)
    val sizeBytes     = ByteBlock.sizeOf(m.getDatatype)
    val isSigned      = ByteBlock.signedFlag(m.getDatatype)
    val compuMethod   = compuMethods(m.getConversion)
    val units         = compuMethod.getUnit
    val dp            = a2l.getFormat(m)._2
    val coeffs        = compuMethod.getCoeffs

    val a = coeffs.getF / coeffs.getB
    val b = coeffs.getC / coeffs.getB

    actualValueTemplate(
      name = name,
      a2lAddress = address.toHexString,
      sizeBytes = sizeBytes.toString,
      dataA = a.toString,
      dataB = b.toString,
      memorySegmentPrefix = segmentPrefix,
      units = units,
      dp = dp.toString,
      isSigned = isSigned,
      indent = 1
    )
  }

}

object Measurement2UserChannel {

  private def actualValuesTemplate(actualValues: Seq[String]): String = {
    s"""<?xml version="1.0" standalone="yes"?>
       |<ActualValues>
       |${actualValues.mkString("\n")}
       |</ActualValues>""".stripMargin
  }

  private def actualValueTemplate(
      name: String,
      a2lAddress: String,
      sizeBytes: String,
      dataA: String,
      dataB: String,
      memorySegmentPrefix: String,
      units: String,
      dp: String,
      isSigned: Boolean,
      indent: Int
  ): String = {

    def signedParam: String = if (isSigned) """ Signed="1"""" else ""

    def indents(p: Int): String = "  ".repeat(indent + p)
    // format: off
    s"""${indents(0)}<ActualValue ReqBlock="$a2lAddress" Size="$sizeBytes" DataA="$dataA" DataB="$dataB" Prefix="$memorySegmentPrefix" Units="$units" RoundingDigits="$dp"$signedParam>
       |${indents(1)}<Text xml:lang="en">$name</Text>
       |${indents(0)}</ActualValue>""".stripMargin
    // format: on
  }

  private val formatExpr = """%(\d+)\.(\d+)""".r

  private def format2DecimalPl(format: String): Int = {
    format match
      case formatExpr(pre, suf) =>
        suf.toInt
  }
}
