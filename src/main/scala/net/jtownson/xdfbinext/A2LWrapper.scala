package net.jtownson.xdfbinext

import net.alenzen.a2l.enums.CharacteristicType
import net.alenzen.a2l.{Asap2File, Unit as A2lUnit, *}
import net.jtownson.xdfbinext.A2LWrapper.getA2L

import java.net.URL
import scala.jdk.CollectionConverters.*
import scala.util.Using

case class A2LWrapper(a2lUrl: URL) {

  val a2l: Asap2File = getA2L(a2lUrl)

  val recordLayouts: Map[String, RecordLayout] = a2l
    .iterator()
    .asScala
    .collect { case r: RecordLayout => r }
    .map(r => r.getName -> r)
    .toMap

  val compuMethods: Map[String, CompuMethod] = a2l
    .iterator()
    .asScala
    .collect { case c: CompuMethod => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val characteristics: Map[String, Characteristic] = a2l
    .iterator()
    .asScala
    .collect { case c: Characteristic => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val measurements: Map[String, Measurement] = a2l
    .iterator()
    .asScala
    .collect { case c: Measurement => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val axisPts: Map[String, AxisPts] = a2l
    .iterator()
    .asScala
    .collect { case c: AxisPts => c }
    .map(cm => cm.getName -> cm)
    .toMap

  val functions: Map[String, Function] = a2l
    .iterator()
    .asScala
    .collect { case f: Function => f }
    .map(f => f.getName -> f)
    .toMap

  def characteristicUsage(name: String): Set[String] = {
    def nn(l: IdentReferenceList): Set[String] = {
      Option(l).fold(Set.empty[String])(_.iterator().asScala.toSet)
    }

    functions.filter { (fnName, fn) =>
      val fnCharacteristics: Set[String] = nn(fn.getDefCharacteristics) ++ nn(fn.getRefCharacteristics)
      fnCharacteristics.contains(name)
    }.keySet
  }
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
