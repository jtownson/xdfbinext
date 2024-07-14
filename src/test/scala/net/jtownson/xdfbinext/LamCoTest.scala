package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.A2L2Dot.{GraphOptions, MeasurementRole}
import org.scalatest.flatspec.AnyFlatSpec

import scala.jdk.CollectionConverters.*

class LamCoTest extends AnyFlatSpec {

  val a2lUrl  = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  val a2l2Dot = new A2L2Dot(a2lUrl)

  it should "graph lamco functions" ignore {
    A2L2Dot.parentGraphWith(
      "Lam_225_001_0",
      a2l2Dot,
      "Lam_225_001_0",
      "Lam_225_001_0",
      _ => true,
      _ => true,
      (_, _) => true
    )
  }
}

object LamCoTest {}
