package co.blocke.scalajack

object NumberConverters {

  implicit class BigIntOps(val bigInt: BigInt) extends AnyVal {

    def toShortExact: Short = {
      val bigIntAsShort: Short = bigInt.toShort
      val bigIntAsShortAsBigInt: BigInt = BigInt(bigIntAsShort)
      if (bigInt == bigIntAsShortAsBigInt) {
        bigIntAsShort
      } else {
        throw new ArithmeticException(s"$bigInt (BigInt) cannot be exactly converted to Short ($bigIntAsShort)")
      }
    }

  }

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

    def toIntExact: Int = {
      val longAsInt: Int = long.toInt
      val longAsIntAsLong: Long = longAsInt.toLong
      if (long == longAsIntAsLong) {
        longAsInt
      } else {
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Int ($longAsInt)")
      }
    }

    def toShortExact: Short = {
      val longAsShort: Short = long.toShort
      val longAsShortAsLong: Long = longAsShort.toLong
      if (long == longAsShortAsLong) {
        longAsShort
      } else {
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Short ($longAsShort)")
      }
    }

  }

}
