package net.jtownson.xdfbinext.bace

import net.jtownson.xdfbinext.a2l.{CurveType, MapType}
import net.jtownson.xdfbinext.bace.BaceDSL.MeasurementType.InMeasurement

object BaceDSL {
  trait MeasurementType[T] {
    def read: T

    def write(t: T): MeasurementType[T]
  }

  extension (input: BigDecimal) {
    def -->(m: MeasurementType[BigDecimal]): MeasurementType[BigDecimal] = {
      m.write(input)
    }
    def -->[Y](m: CurveType[BigDecimal, Y]): MeasurementType[Y] = {
      val z = m.apply(input)
      InMeasurement(z)
    }
  }

  extension [X, Y](input: (BigDecimal, BigDecimal)) {
    def -->[Z](m: MapType[BigDecimal, BigDecimal, Z]): MeasurementType[Z] = {
      val (x, y) = input
      val z      = m.apply(x, y)
      InMeasurement(z)
    }
  }

  extension (input: String) {
    def -->(m: MeasurementType[String]): MeasurementType[String] = {
      m.write(input)
    }
  }

  object MeasurementType {

    case class InMeasurement[T](t: T) extends MeasurementType[T] {
      override def read: T = t

      override def write(t: T): MeasurementType[T] = throw new UnsupportedOperationException()
    }

    case class OutMeasurement[T]() extends MeasurementType[T] {
      var t: T = _

      override def read: T = t

      override def write(tt: T): MeasurementType[T] = {
        t = tt
        this
      }
    }

    extension [X](input: MeasurementType[X]) {

      def -->(c: BigDecimal): MeasurementType[BigDecimal] = {
        InMeasurement(c)
      }

      def -->(s: String): MeasurementType[String] = {
        InMeasurement(s)
      }

      def -->[Y](m: CurveType[X, Y]): MeasurementType[Y] = {
        val x: X = input.read
        val z    = m.apply(x)
        InMeasurement(z)
      }
      def -->(m: MeasurementType[X]): MeasurementType[X] = {
        m.write(input.read)
      }
    }

    extension [X, Y](input: (MeasurementType[X], MeasurementType[Y])) {
      def -->[Z](m: MapType[X, Y, Z]): MeasurementType[Z] = {
        val (xm, ym) = input
        val x        = xm.read
        val y        = ym.read
        val z        = m.apply(x, y)
        InMeasurement(z)
      }
    }
  }
}
