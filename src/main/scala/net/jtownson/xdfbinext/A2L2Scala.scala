package net.jtownson.xdfbinext

import net.alenzen.a2l.IdentReferenceList
import net.jtownson.xdfbinext.A2L2Scala.zip3

import java.io.PrintStream
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

class A2L2Scala(val a2l: A2LWrapper, outputPath: Path) {

  def callGen(fnName: String, out: PrintStream): Unit = {
    val fn              = a2l.functions(fnName)
    val inMeasurements  = nfm(fn.getInMeasurments)
    val outMeasurements = nfm(fn.getOutMeasurments)
    val locMeasurements = nfm(fn.getLocMeasurments)
    val characteristics = nfm(fn.getDefCharacteristics) ++ nfm(fn.getRefCharacteristics)

    val characteristicParamNames = characteristics
      .filter(a2l.characteristics.contains)

    val characteristicParamTypes = getCharacteristicParamTypes(characteristicParamNames)
    val characteristicValidNames = characteristicParamNames.map(validName)
    val characteristicParamDefs = zip3(characteristicParamNames, characteristicValidNames, characteristicParamTypes)
      .map((name, validName, tpe) => s"val $validName: $tpe = a2lBin.readCharacteristicWithCast(\"$name\")")
      .mkString("\n")

    val inMeasurementValidNames = inMeasurements.map(validName)
    val inMeasurementDefs = inMeasurements
      .zip(inMeasurementValidNames)
      .map((mName, mValidName) => s"val $mValidName: InMeasurement = a2lBin.measurement(\"$mName\")")
      .mkString("\n")
    val inMeasurementParams = inMeasurementValidNames
      .map(mValidName => s"$mValidName: A2LMeasurement")

    val outMeasurementValidNames = outMeasurements.map(validName)
    val outMeasurementDefs = outMeasurements
      .zip(outMeasurementValidNames)
      .map((mName, mValidName) => s"val $mValidName: OutMeasurement = a2lBin.measurement(\"$mName\")")
      .mkString("\n")
    val outMeasurementParams = outMeasurementValidNames
      .map(mValidName => s"$mValidName: A2LMeasurement")

    val paramsInner = (inMeasurementValidNames ++ characteristicValidNames ++ outMeasurementValidNames).mkString(", ")

    val fnStr = s"""
                   |def $fnName(a2lBin: A2LBinAdapter): Unit = {
                   |
                   |$characteristicParamDefs
                   |$inMeasurementDefs
                   |$outMeasurementDefs
                   |
                   |  $fnName($paramsInner)
                   |}
                   |""".stripMargin

    out.println(fnStr)
  }

  def fnGen(fnName: String): Unit = {
    fnGen(fnName, System.out)
  }

  def fnGen(fnName: String, out: PrintStream): Unit = {

    val fn = a2l.functions(fnName)

    val subFns = subFunctionNames(fn.getSubFunctions)

    if subFns.isEmpty then leafFnGen(fnName, out)
    else
      out.println(commonImports)
      out.println(s"object ${validName(fnName)} {")
      subFns.foreach(subFn => fnGen(subFn, out))
      out.println("}")

  }

  def nfm(l: IdentReferenceList): Seq[String] = {
    Option(l).fold(Seq.empty[String])(_.iterator().asScala.toSeq)
  }

  private def getCharacteristicParamTypes(characteristicParamNames: Seq[String]): Seq[String] = {
    characteristicParamNames
      .map { cName =>
        val cc = a2l.characteristics(cName)
        a2l.characteristicTypeFold[String](
          cc,
          fString = () => "String",
          fNumber = () => "BigDecimal",
          fStringArr = () => "Array[String]",
          fNumberArr = () => "Array[BigDecimal]",
          fNumberString = () => "CurveType[BigDecimal, String]",
          fNumberNumber = () => "CurveType[BigDecimal, BigDecimal]",
          fStringNumber = () => "CurveType[String, BigDecimal]",
          fStringString = () => "CurveType[String, String]",
          fNumberNumberNumber = () => "MapType[BigDecimal, BigDecimal, BigDecimal]",
          fNumberNumberString = () => "MapType[BigDecimal, String, BigDecimal]",
          fNumberStringNumber = () => "MapType[BigDecimal, String, BigDecimal]",
          fNumberStringString = () => "MapType[BigDecimal, String, String]",
          fStringNumberString = () => "MapType[String, BigDecimal, String]",
          fStringNumberNumber = () => "Map[String, BigDecimal, BigDecimal]",
          fStringStringString = () => "MapType[String, String, String]",
          fStringStringNumber = () => "MapType[String, String, BigDecimal]"
        )
      }
  }

  private def leafFnGen(fnName: String, out: PrintStream): Unit = {
    fnDefinitionGen(fnName, out)
    callGen(fnName, out)
  }

  private def fnDefinitionGen(fnName: String, out: PrintStream): Unit = {
    val fn              = a2l.functions(fnName)
    val inMeasurements  = nfm(fn.getInMeasurments)
    val outMeasurements = nfm(fn.getOutMeasurments)
    val locMeasurements = nfm(fn.getLocMeasurments)
    val characteristics = nfm(fn.getDefCharacteristics) ++ nfm(fn.getRefCharacteristics)

    val characteristicParamNames = characteristics
      .filter(a2l.characteristics.contains)

    val characteristicParamTypes = getCharacteristicParamTypes(characteristicParamNames)

    val characteristicParams = characteristicParamNames
      .map(validName)
      .zip(characteristicParamTypes)
      .map((name, tpe) => s"$name: $tpe")

    val inMeasurementParams = inMeasurements.map(validName).map(mName => s"$mName: InMeasurement")

    val outMeasurementParams = outMeasurements.map(validName).map(mName => s"$mName: OutMeasurement")

    val params = inMeasurementParams ++ characteristicParams ++ outMeasurementParams

    val paramStr = params.mkString(", ")

    val fnStr =
      s"""
         |def $fnName($paramStr): Unit = {
         | ???
         |}""".stripMargin

    out.println(fnStr)
  }

  private def validName(s: String): String = s.replaceAll("\\.", "_")

  private def subFunctionNames(fnName: String): Seq[String] = {
    val fn = a2l.functions(fnName)

    subFunctionNames(fn.getSubFunctions)
  }

  private def subFunctionNames(l: IdentReferenceList): Seq[String] = {
    Option(l).fold(Vector.empty[String])(_.iterator().asScala.toVector)
  }

  private val commonImports =
    """
      |package net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B
      |import net.jtownson.xdfbinext.bace.BaceDSL.*
      |import net.jtownson.xdfbinext.A2LBinAdapter
      |import net.jtownson.xdfbinext.a2l.A2LMeasurement.{InMeasurement, OutMeasurement}
      |import net.jtownson.xdfbinext.a2l.{CurveType, MapType, A2LMeasurement}
      |""".stripMargin

}

object A2L2Scala {
  def zip3[A, B, C](a: Seq[A], b: Seq[B], c: Seq[C]): Seq[(A, B, C)] = {
    a.zip(b).zip(c).map { case ((a, b), c) => (a, b, c) }
  }
}
