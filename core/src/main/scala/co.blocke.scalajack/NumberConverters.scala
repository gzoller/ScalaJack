package co.blocke.scalajack

object NumberConverters {

  implicit class DoubleOps(val double: Double) extends AnyVal {

    def toFloatExact: Float = {
      val doubleAsFloat: Float = double.floatValue
      val doubleAsFloatAsDouble: Double = doubleAsFloat.doubleValue
      if (double == doubleAsFloatAsDouble) {
        doubleAsFloat
      } else {
        throw new ArithmeticException(s"$double (Double) cannot be exactly converted to $doubleAsFloat (Float)")
      }
    }

  }

  implicit class LongOps(val long: Long) extends AnyVal {

    def toFloatExact: Float = {
      val longAsFloat: Float = long.floatValue
      val longAsFloatAsLong: Long = longAsFloat.longValue
      if (long == longAsFloatAsLong) {
        longAsFloat
      } else {
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Float ($longAsFloat)")
      }
    }

  }

}
