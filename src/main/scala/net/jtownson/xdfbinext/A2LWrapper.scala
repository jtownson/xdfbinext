package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.{CharacteristicType, DataType}
import net.alenzen.a2l.{Asap2File, Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2LWrapper.getA2L

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Using

case class A2LWrapper(a2lUrl: URL) {

  val a2l: Asap2File = getA2L(a2lUrl)

  val recordLayouts: Map[String, RecordLayout] = collectWithPf[RecordLayout](_.getName)

  val compuMethods: Map[String, CompuMethod] = collectWithPf[CompuMethod](_.getName)

  val compuVTabs: Map[String, CompuVTab] = collectWithPf[CompuVTab](_.getName)

  val characteristics: Map[String, Characteristic] = collectWithPf[Characteristic](_.getName)

  val measurements: Map[String, Measurement] = collectWithPf[Measurement](_.getName)

  val axisPts: Map[String, AxisPts] = collectWithPf[AxisPts](_.getName)

  val functions: Map[String, Function] = collectWithPf[Function](_.getName)

  def characteristicUsage(name: String): Set[String] = {
    def nn(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    functions.filter { (fnName, fn) =>
      val fnCharacteristics: Set[String] = nn(fn.getDefCharacteristics) ++ nn(fn.getRefCharacteristics)
      fnCharacteristics.contains(name)
    }.keySet
  }

  private def getType(r: RecordLayout): DataType = {
    Option(r.getFunctionValues).map(_.getDataType).getOrElse(r.getAxisPtsX.getDatatype)
  }

  def getType(c: Characteristic): DataType = {
    val layout: RecordLayout = recordLayouts(c.getDeposit)
    getType(layout)
  }

  def getType(a: AxisPts): DataType = {
    val layout: RecordLayout = recordLayouts(a.getDeposit)
    getType(layout)
  }

  def getFormat(c: Characteristic): (Int, Int) = {
    getFormat(c.getFormat)
  }

  def getFormat(a: AxisPts): (Int, Int) = {
    getFormat(a.getFormat)
  }

  def getCellCount(c: Characteristic): Int = {
    if (c.getType == CharacteristicType.VALUE) {
      1
    } else if (c.getType == CharacteristicType.CURVE) {
      getXAxis(c).getMaxAxisPoints.toInt
    } else if (c.getType == CharacteristicType.MAP) {
      getXAxis(c).getMaxAxisPoints.toInt * getYAxis(c).getMaxAxisPoints.toInt
    } else if (c.getType == CharacteristicType.VAL_BLK) {
      c.getNumber.toInt
    } else {
      ???
    }
  }

  def getCellCount(a: AxisPts): Int = {
    a.getMaxAxisPoints.toInt
  }

  def getXAxis(c: Characteristic): AxisPts = {
    // ABSOLUTE storage TODO
    getAxis(c, 0)
  }

  def getYAxis(c: Characteristic): AxisPts = {
    getAxis(c, 1)
  }

  private def getAxis(c: Characteristic, i: Int): AxisPts = {
    val xAxisDescr = c.getAxisDescriptions.asScala(i)
    val xAxisRef   = xAxisDescr.getAxisPoints_ref
    if (!axisPts.contains(xAxisRef))
      println("missing")
    axisPts(xAxisRef)
  }

  private val formatExpr = """%(\d+)\.(\d+)""".r

  private def getFormat(format: String): (Int, Int) = {
    format match
      case formatExpr(len, dp) =>
        (len.toInt, dp.toInt)
  }

  private def collectWithPf[T: ClassTag](id: T => String): Map[String, T] =
    a2l
      .iterator()
      .asScala
      .collect { case t: T => t }
      .map(n => id(n) -> n)
      .toMap
}

object A2LWrapper {
  private def getA2L(a2lUrl: URL): Asap2File = {
    Using.resource(a2lUrl.openStream()) { i =>
      val parser: Asap2Parser = new Asap2Parser(i)
      parser.parse()
    }
  }

  def characteristicFold[T](
      c: Characteristic,
      fValue: Characteristic => T,
      fCurve: Characteristic => T,
      fMap: Characteristic => T
  ): T = {
    if (c.getType == CharacteristicType.VALUE)
      fValue(c)
    else if (c.getType == CharacteristicType.CURVE)
      fCurve(c)
    else if (c.getType == CharacteristicType.MAP)
      fMap(c)
    else
      throw new IllegalStateException(s"Unsupported characteristic type: ${c.getType}")
  }

  def getObjectDescription(name: String, default: String): String = {
    BmwTchDescriptions.table.getOrElse(name, default)
  }

}
