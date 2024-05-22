package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.{ConversionType, DataType}
import net.alenzen.a2l.{Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2L2XDF.*
import net.jtownson.xdfbinext.A2LWrapper.{characteristicFold, getObjectDescription}
import net.jtownson.xdfbinext.XdfSchema.{CategoryMem, XdfModel}
import net.jtownson.xdfbinext.a2l.FormulaExpressionInverse

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.HALF_UP

class A2L2XDF(a2lUrl: URL, offset: Long = 0x9000000, xdfModel: XdfModel) {

  private val a2l = A2LWrapper(a2lUrl)

  private val characteristics = a2l.characteristics
  private val compuMethods    = a2l.compuMethods
  private val axisPts         = a2l.axisPts
  private val catMap          = xdfModel.xdfHeader.categories.map(cat => cat.name -> cat).toMap

  private def offsetAddress(a2lAddress: Long): String = {
    s"0x${(a2lAddress - offset).toHexString}"
  }

  def characteristic2XDF(namePredicate: String => Boolean): Seq[String] = {
    val applicableCharacteristics = characteristics.filter((n, _) => namePredicate(n)).values

    applicableCharacteristics.map(characteristic2XDF).toSeq
  }

  private def characteristic2XDF(c: Characteristic): String = {
    characteristicFold(c, value2Xdf, curve2Xdf, map2Xdf)
  }

  private def getTypeFlag(c: Characteristic): Int = {
    getTypeFlag(a2l.recordLayouts(c.getDeposit))
  }

  private def getTypeFlag(a: AxisPts): Int = {
    getTypeFlag(a2l.recordLayouts(a.getDeposit))
  }

  private def getSizeBits(c: Characteristic): Int = {
    getSizeBits(a2l.recordLayouts(c.getDeposit))
  }

  private def getSizeBits(a: AxisPts): Int = {
    getSizeBits(a2l.recordLayouts(a.getDeposit))
  }

  private def getTypeFlag(r: RecordLayout): Int = {
    val dataType = Option(r.getFunctionValues).map(_.getDataType).getOrElse(r.getAxisPtsX.getDatatype)
    if (dataType == DataType.UBYTE || dataType == DataType.UWORD || dataType == DataType.ULONG)
      0
    else if (dataType == DataType.SBYTE || dataType == DataType.SWORD || dataType == DataType.SLONG)
      XdfSchema.signedFlag
    else if (dataType == DataType.FLOAT32_IEEE || dataType == DataType.FLOAT64_IEEE)
      XdfSchema.floatingPointFlag
    else
      throw new IllegalStateException(s"Unsupported data type: $dataType")
  }

  private def getSizeBits(r: RecordLayout): Int = {
    val dataType = Option(r.getFunctionValues).map(_.getDataType).getOrElse(r.getAxisPtsX.getDatatype)
    if (dataType == DataType.SBYTE || dataType == DataType.UBYTE)
      8
    else if (dataType == DataType.SWORD || dataType == DataType.UWORD)
      16
    else if (dataType == DataType.SLONG || dataType == DataType.ULONG || dataType == DataType.FLOAT32_IEEE)
      32
    else if (dataType == DataType.A_INT64 || dataType == DataType.A_UINT64 || dataType == DataType.FLOAT64_IEEE)
      64
    else
      throw new IllegalStateException(s"Unsupported data type: $dataType")

  }

  private def getCategoryMems(c: Characteristic): List[CategoryMem] = {
    val refFns = a2l.characteristicUsage(c.getName)

    val cats =
      if (c.getName == "KL_AUSY_TURB")
        List(catMap("BMW_MOD_TchCtr_Pwr2Pos_10ms"))
      else
        refFns.flatMap(catMap.get).filter(_.name != "BMW_MOD_AusyTurb_Seg").toList

    cats.indices.map(i => CategoryMem(i, cats(i))).toList
  }

  def value2Xdf(c: Characteristic): String = {
    val title        = c.getName
    val description  = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod  = compuMethods(c.getConversion)
    val units        = compuMethod.getUnit
    val decimalPl    = format2DecimalPl(compuMethod.getFormat)
    val outputType   = "1"
    val min          = BigDecimal(c.getLowerLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val max          = BigDecimal(c.getUpperLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val eq           = getFormula(c)
    val typeFlag     = getTypeFlag(c)
    val address      = offsetAddress(c.getAddress)
    val elSizeBits   = getSizeBits(c)
    val categoryMems = getCategoryMems(c)

    xdfValueTemplate(
      title,
      description,
      units,
      decimalPl,
      outputType,
      min,
      max,
      eq,
      typeFlag,
      address,
      elSizeBits,
      categoryMems
    )
  }

  def curve2Xdf(c: Characteristic): String = {
    val title        = c.getName
    val description  = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod  = compuMethods(c.getConversion)
    val units        = compuMethod.getUnit
    val decimalPl    = format2DecimalPl(compuMethod.getFormat)
    val outputType   = "1"
    val min          = BigDecimal(c.getLowerLimit).setScale(decimalPl, HALF_UP).toString
    val max          = BigDecimal(c.getUpperLimit).setScale(decimalPl, HALF_UP).toString
    val eq           = getFormula(c)
    val typeFlag     = getTypeFlag(c)
    val address      = offsetAddress(c.getAddress)
    val elSizeBits   = getSizeBits(c)
    val categoryMems = getCategoryMems(c)

    val xAxisDescr  = c.getAxisDescriptions.asScala(0)
    val xAxisRef    = xAxisDescr.getAxisPoints_ref
    val xAxis       = axisPts(xAxisRef)
    val maybeXCompu = compuMethods.get(xAxis.getConversion)
    val xUnits      = maybeXCompu.map(_.getUnit).getOrElse("-")
    val xTitle      = xAxis.getName
    val xDesc       = xAxis.getLongIdentifier
    val xDp         = format2DecimalPl(xAxis.getFormat)
    val xMin        = BigDecimal(xAxisDescr.getLowerLimit).setScale(xDp.toInt, HALF_UP).toString
    val xMax        = BigDecimal(xAxisDescr.getUpperLimit).setScale(xDp.toInt, HALF_UP).toString
    val xAddr       = offsetAddress(xAxis.getAddress)
    val xElSzBits   = getSizeBits(xAxis)
    val xTypeFlag   = getTypeFlag(xAxis)
    val xColCount   = xAxis.getMaxAxisPoints.toString
    val xEq         = getFormula(xAxis)

    val sb: StringBuilder = new StringBuilder()

    sb.append(
      xdfCurveTemplate(
        title,
        description,
        units,
        decimalPl,
        outputType,
        min,
        max,
        eq,
        typeFlag,
        address,
        elSizeBits,
        xAddr,
        xElSzBits,
        xTypeFlag,
        xDp,
        xColCount,
        xUnits,
        xEq,
        categoryMems
      )
    )

    sb.append(
      xdfAxisTemplate(xTitle, xDesc, xAddr, xColCount, xUnits, xDp, xMin, xMax, xEq, xTypeFlag, xElSzBits, categoryMems)
    )

    sb.toString
  }

  private def getFormula(c: Characteristic): String = {
    val compuMethod = compuMethods(c.getConversion)
    if (compuMethod.getConversionType == ConversionType.RAT_FUNC) {
      val coeffs = compuMethods(c.getConversion).getCoeffs
      FormulaExpressionInverse.toFormulaInv(coeffs.getA, coeffs.getB, coeffs.getC, coeffs.getD, coeffs.getE, coeffs.getF)
    } else {
      "X"
    }
  }

  private def getFormula(axis: AxisPts): String = {
    val coeffs = compuMethods(axis.getConversion).getCoeffs
    FormulaExpressionInverse.toFormulaInv(coeffs.getA, coeffs.getB, coeffs.getC, coeffs.getD, coeffs.getE, coeffs.getF)
  }

  private def map2Xdf(c: Characteristic): String = {
    val title        = c.getName
    val description  = getObjectDescription(c.getName, c.getLongIdentifier)
    val compuMethod  = compuMethods(c.getConversion)
    val units        = compuMethod.getUnit
    val decimalPl    = format2DecimalPl(compuMethod.getFormat)
    val outputType   = "1"
    val min          = BigDecimal(c.getLowerLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val max          = BigDecimal(c.getUpperLimit).setScale(decimalPl.toInt, HALF_UP).toString
    val typeFlag     = getTypeFlag(c)
    val address      = offsetAddress(c.getAddress)
    val elSizeBits   = getSizeBits(c)
    val eq           = getFormula(c)
    val categoryMems = getCategoryMems(c)

    val xAxisDescr  = c.getAxisDescriptions.asScala(0)
    val xAxisRef    = xAxisDescr.getAxisPoints_ref
    val xAxis       = axisPts(xAxisRef)
    val maybeXCompu = compuMethods.get(xAxis.getConversion)
    val xUnits      = maybeXCompu.map(_.getUnit).getOrElse("-")
    val xTitle      = xAxis.getName
    val xDesc       = xAxis.getLongIdentifier
    val xDp         = format2DecimalPl(xAxis.getFormat)
    val xMin        = BigDecimal(xAxisDescr.getLowerLimit).setScale(xDp, HALF_UP).toString
    val xMax        = BigDecimal(xAxisDescr.getUpperLimit).setScale(xDp, HALF_UP).toString
    val xAddr       = offsetAddress(xAxis.getAddress)
    val xElSzBits   = getSizeBits(xAxis)
    val xTypeFlag   = getTypeFlag(xAxis)
    val xColCount   = xAxis.getMaxAxisPoints.toString
    val xEq         = getFormula(xAxis)

    val yAxisDescr  = c.getAxisDescriptions.asScala(1)
    val yAxisRef    = yAxisDescr.getAxisPoints_ref
    val yAxis       = axisPts(yAxisRef)
    val maybeYCompu = compuMethods.get(yAxis.getConversion)
    val yUnits      = maybeYCompu.map(_.getUnit).getOrElse("-")
    val yTitle      = yAxis.getName
    val yDesc       = yAxis.getLongIdentifier
    val yDp         = format2DecimalPl(yAxis.getFormat)
    val yMin        = BigDecimal(yAxisDescr.getLowerLimit).setScale(yDp, HALF_UP).toString
    val yMax        = BigDecimal(yAxisDescr.getUpperLimit).setScale(yDp, HALF_UP).toString
    val yAddr       = offsetAddress(yAxis.getAddress)
    val yElSzBits   = getSizeBits(yAxis)
    val yTypeFlag   = getTypeFlag(yAxis)
    val yColCount   = yAxis.getMaxAxisPoints.toString
    val yEq         = getFormula(yAxis)

    val sb = new StringBuilder

    sb.append(
      xdfMapTemplate(
        title,
        description,
        units,
        decimalPl,
        outputType,
        min,
        max,
        eq,
        typeFlag,
        address,
        elSizeBits,
        xAddr,
        xElSzBits,
        xTypeFlag,
        xDp,
        xColCount,
        xUnits,
        xEq,
        yAddr,
        yElSzBits,
        yTypeFlag,
        yDp,
        yColCount,
        yUnits,
        yEq,
        categoryMems
      )
    )

    sb.append(
      xdfAxisTemplate(xTitle, xDesc, xAddr, xColCount, xUnits, xDp, xMin, xMax, xEq, xTypeFlag, xElSzBits, categoryMems)
    )

    sb.append(
      xdfAxisTemplate(yTitle, yDesc, yAddr, yColCount, yUnits, yDp, yMin, yMax, yEq, yTypeFlag, yElSzBits, categoryMems)
    )

    sb.toString
  }

}

object A2L2XDF {

  // format: off
  private def xdfValueTemplate(
                        title: String,
                        description: String,
                        units: String,
                        decimalPl: Int,
                        outputType: String,
                        min: String,
                        max: String,
                        equation: String,
                        typeFlag: Int,
                        address: String,
                        elSzBits: Int,
                        categoryMems: List[CategoryMem]
                      ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
       |    <title>$title</title>
       |    <description>$description</description>
       |    ${categoryMems.map(catMem)}
       |    <XDFAXIS id="x" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="8" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>-</units>
       |      <indexcount>1</indexcount>
       |      <decimalpl>-1</decimalpl>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <LABEL index="0" value="0.000000" />
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="y" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="8" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>-</units>
       |      <indexcount>1</indexcount>
       |      <decimalpl>-1</decimalpl>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <LABEL index="0" value="0.000000" />
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="z">
       |      <EMBEDDEDDATA ${typeFlagAll(typeFlag)} mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$units</units>
       |      <decimalpl>$decimalPl</decimalpl>
       |      <min>$min</min>
       |      <max>$max</max>
       |      <outputtype>$outputType</outputtype>
       |      <MATH equation="$equation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |  </XDFTABLE>
       |""".stripMargin
  }


  private def xdfCurveTemplate(
                        title: String,
                        description: String,
                        units: String,
                        decimalPl: Int,
                        outputType: String,
                        min: String,
                        max: String,
                        equation: String,
                        typeFlag: Int,
                        address: String,
                        elSzBits: Int,
                        xAddr: String,
                        xElSzBits: Int,
                        xTypeFlag: Int,
                        xDp: Int,
                        xColCount: String,
                        xUnits: String,
                        xEquation: String,
                        categoryMems: List[CategoryMem]
                      ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
       |    <title>$title</title>
       |    <description>$description</description>
       |    ${categoryMems.map(catMem).mkString}
       |    <XDFAXIS id="x" uniqueid="0x0">
       |      <EMBEDDEDDATA ${typeFlagAll(xTypeFlag)} mmedaddress="$xAddr" mmedelementsizebits="$xElSzBits" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$xUnits</units>
       |      <indexcount>$xColCount</indexcount>
       |      <decimalpl>$xDp</decimalpl>
       |      <embedinfo type="1" />
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <MATH equation="$xEquation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="y" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="8" mmedrowcount="1" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>-</units>
       |      <indexcount>1</indexcount>
       |      <decimalpl>-1</decimalpl>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <LABEL index="0" value="0.000000" />
       |      <MATH equation="X*1">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="z">
       |      <EMBEDDEDDATA ${typeFlagAll(typeFlag)} mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="1" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$units</units>
       |      <decimalpl>$decimalPl</decimalpl>
       |      <min>$min</min>
       |      <max>$max</max>
       |      <outputtype>1</outputtype>
       |      <MATH equation="$equation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |  </XDFTABLE>
       |""".stripMargin
  }

  private def xdfMapTemplate(
                      title: String,
                      description: String,
                      units: String,
                      decimalPl: Int,
                      outputType: String,
                      min: String,
                      max: String,
                      equation: String,
                      typeFlag: Int,
                      address: String,
                      elSzBits: Int,
                      xAddr: String,
                      xElSzBits: Int,
                      xTypeFlag: Int,
                      xDp: Int,
                      xColCount: String,
                      xUnits: String,
                      xEquation: String,
                      yAddr: String,
                      yElSzBits: Int,
                      yTypeFlag: Int,
                      yDp: Int,
                      yColCount: String,
                      yUnits: String,
                      yEquation: String,
                      categoryMems: List[CategoryMem]
                    ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
       |    <title>$title</title>
       |    <description>$description</description>
       |    ${categoryMems.map(catMem).mkString}
       |    <XDFAXIS id="x" uniqueid="0x0">
       |      <EMBEDDEDDATA ${typeFlagAll(xTypeFlag)} mmedaddress="$xAddr" mmedelementsizebits="$xElSzBits" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$xUnits</units>
       |      <indexcount>$xColCount</indexcount>
       |      <decimalpl>$xDp</decimalpl>
       |      <embedinfo type="1" />
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <MATH equation="$xEquation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="y" uniqueid="0x0">
       |      <EMBEDDEDDATA ${typeFlagAll(yTypeFlag)} mmedaddress="$yAddr" mmedelementsizebits="$yElSzBits" mmedrowcount="$yColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$yUnits</units>
       |      <indexcount>$yColCount</indexcount>
       |      <decimalpl>$yDp</decimalpl>
       |      <embedinfo type="1" />
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <MATH equation="$yEquation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="z">
       |      <EMBEDDEDDATA ${typeFlagAll(typeFlag)} mmedaddress="$address" mmedelementsizebits="$elSzBits" mmedrowcount="$yColCount" mmedcolcount="$xColCount" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$units</units>
       |      <decimalpl>$decimalPl</decimalpl>
       |      <min>$min</min>
       |      <max>$max</max>
       |      <outputtype>1</outputtype>
       |      <MATH equation="$equation">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |  </XDFTABLE>
       |""".stripMargin
  }

  private def label(i: Int): String = s"""<LABEL index="$i" value="0.00" />"""


  private def catMem(cm: CategoryMem): String = 
    s"""<CATEGORYMEM index="${cm.index}" category="${Integer.parseInt(cm.category.index.drop(2), 16) + 1}" />"""
    
  private def typeFlagAll(typeFlag: Int): String = {
    if (typeFlag == XdfSchema.signedFlag)
      """mmedtypeflags="0x01""""
    else if (typeFlag == XdfSchema.floatingPointFlag)
      """mmedtypeflags="0x10000""""
    else ""
  }

  private val newline = "\n"

  private def xdfAxisTemplate(
                       title: String,
                       description: String,
                       address: String,
                       count: String,
                       units: String,
                       decimalPl: Int,
                       min: String,
                       max: String,
                       eq: String,
                       typeFlag: Int,
                       elSizeBits: Int,
                       categoryMems: List[CategoryMem]
                     ): String = {
    s"""  <XDFTABLE uniqueid="0x0" flags="0x0">
       |    <title>$title</title>
       |    <description>$description</description>
       |    ${categoryMems.map(catMem).mkString}
       |    <XDFAXIS id="x" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="16" mmedmajorstridebits="-32" mmedminorstridebits="0" />
       |      <indexcount>$count</indexcount>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      ${(0 until count.toInt).map(label).mkString}
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="y" uniqueid="0x0">
       |      <EMBEDDEDDATA mmedelementsizebits="16" mmedmajorstridebits="-32" mmedminorstridebits="0" />
       |      <indexcount>1</indexcount>
       |      <datatype>0</datatype>
       |      <unittype>0</unittype>
       |      <DALINK index="0" />
       |      <LABEL index="0" value="0.00" />
       |      <MATH equation="X">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |    <XDFAXIS id="z">
       |      <EMBEDDEDDATA ${typeFlagAll(typeFlag)} mmedaddress="$address" mmedelementsizebits="$elSizeBits" mmedrowcount="1" mmedcolcount="$count" mmedmajorstridebits="0" mmedminorstridebits="0" />
       |      <units>$units</units>
       |      <decimalpl>$decimalPl</decimalpl>
       |      <min>$min</min>
       |      <max>$max</max>
       |      <outputtype>1</outputtype>
       |      <MATH equation="$eq">
       |        <VAR id="X" />
       |      </MATH>
       |    </XDFAXIS>
       |  </XDFTABLE>
       |""".stripMargin
  }

  private val formatExpr = """%(\d+)\.(\d+)""".r

  private def format2DecimalPl(format: String): Int = {
    format match
      case formatExpr(pre, suf) =>
        suf.toInt
  }
}