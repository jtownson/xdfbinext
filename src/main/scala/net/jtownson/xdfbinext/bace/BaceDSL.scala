package net.jtownson.xdfbinext.bace

import net.jtownson.xdfbinext.a2l.{A2LMeasurement, CurveType, MapType}

object BaceDSL {

  trait Input[T]

  given bigDecimalInput: Input[BigDecimal]      = new Input[BigDecimal] {}
  given stringInput: Input[String]              = new Input[String] {}
  given measurementInput: Input[A2LMeasurement] = new Input[A2LMeasurement] {}

  extension [I: Input](i: I)(using ev: Conversion[I, BigDecimal])
    def -->(m: A2LMeasurement): A2LMeasurement =
      m.write1(ev(i))
      m

  extension [I: Input, X: Input, Z: Input](i: I)(using ev: Conversion[I, X])
    def -->(m: CurveType[X, Z]): Z =
      m(ev(i))

  given bd2BD: Conversion[BigDecimal, BigDecimal] with
    override def apply(x: BigDecimal): BigDecimal = x

  given measurement2BD: Conversion[A2LMeasurement, BigDecimal] with
    override def apply(m: A2LMeasurement): BigDecimal = m.read1

  given string2BD: Conversion[String, BigDecimal] with
    override def apply(s: String): BigDecimal = {
      if "true" == s.toLowerCase then BigDecimal(1)
      else if "false" == s.toLowerCase then BigDecimal(0)
      else
        throw new IllegalStateException(
          s"Not convertible to big decimal: $s. " +
            s"The conversion assumes the string is either true or false and convertible to 1 or 0 respectively."
        )
    }

  given bd2Boolean: Conversion[BigDecimal, Boolean] with
    def apply(t: BigDecimal): Boolean = {
      if t == 0 then false
      else if t.toIntExact == 1 then true
      else throw new IllegalStateException(s"Not a boolean: $t")
    }

  given measurement2Boolean: Conversion[A2LMeasurement, Boolean] with
    override def apply(x: A2LMeasurement): Boolean = bd2Boolean(x.read1)

  given str2Boolean: Conversion[String, Boolean] with
    override def apply(s: String): Boolean =
      if "false" == s.toLowerCase then false
      else if "true" == s.toLowerCase then true
      else throw new IllegalStateException(s"Not a boolean: $s")

  type BD2 = (BigDecimal, BigDecimal)
  type BD3 = (BigDecimal, BigDecimal, BigDecimal)

  extension [I: Input](i: I)(using ev: Conversion[I, BD2])
    def -->(m: MapType[BigDecimal, BigDecimal, BigDecimal]): BigDecimal =
      val (i1, i2) = ev(i)
      m(i1, i2)

  given input2[A, B](using eva: Input[A], evb: Input[B]): Input[(A, B)]                      = new Input[(A, B)] {}
  given input3[A, B, C](using eva: Input[A], evb: Input[B], evc: Input[C]): Input[(A, B, C)] = new Input[(A, B, C)] {}

  given input2Conv[A, B, C, D](using
      eva: Conversion[A, C],
      evb: Conversion[B, D]
  ): Conversion[(A, B), (C, D)] with
    override def apply(x: (A, B)): (C, D) = (eva(x._1), evb(x._2))

  extension [I: Input, I1: Input, I2: Input, I3: Input, O1: Input](i: I)(using ev: Conversion[I, (I1, I2, I3)]) {
    def -->(block: Block31[I1, I2, I3, O1]): O1 =
      val (i1, i2, i3) = ev(i)
      block(i1, i2, i3)
  }

  given input3Conv[A, B, C, D, E, F](using
      eva: Conversion[A, D],
      evb: Conversion[B, E],
      evc: Conversion[C, F]
  ): Conversion[(A, B, C), (D, E, F)] with
    override def apply(x: (A, B, C)): (D, E, F) = (eva(x._1), evb(x._2), evc(x._3))

  trait Block31[I1: Input, I2: Input, I3: Input, O1: Input] {
    def apply(i1: I1, i2: I2, i3: I3): O1
  }

}
