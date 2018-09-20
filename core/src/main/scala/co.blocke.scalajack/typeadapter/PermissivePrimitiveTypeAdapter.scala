package co.blocke.scalajack
package typeadapter

import javaprimitives._

object PermissivePrimitiveTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Boolean]) {
      BooleanTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Boolean]) {
      JavaBooleanTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Byte]) {
      ByteTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Byte]) {
      JavaByteTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Double]) {
      DoubleTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Double]) {
      JavaDoubleTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Float]) {
      FloatTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Float]) {
      JavaFloatTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Int]) {
      IntTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Integer]) {
      JavaIntegerTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Long]) {
      LongTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Long]) {
      JavaLongTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Short]) {
      ShortTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Short]) {
      JavaShortTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[BigInt]) {
      BigIntTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.math.BigInteger]) {
      JavaBigIntegerTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[BigDecimal]) {
      BigDecimalTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.math.BigDecimal]) {
      JavaBigDecimalTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }

  /*
  object ScalaBooleanTypeAdapter extends TypeAdapter[Boolean] {

    override def read(reader: Reader): Boolean =
      reader.peek match {
        case TokenType.True | TokenType.False =>
          reader.readBoolean()
        case TokenType.String =>
          reader.readString() match {
            case "true"  => true
            case "false" => false
            case unknown => throw new IllegalStateException(s"""Unknown boolean: "$unknown"""")
          }
      }

    override def write(value: Boolean, writer: Writer): Unit =
      writer.writeBoolean(value)

  }

  object JavaBooleanTypeAdapter extends TypeAdapter[java.lang.Boolean] {

    override def read(reader: Reader): java.lang.Boolean =
      reader.peek match {
        case TokenType.True | TokenType.False =>
          java.lang.Boolean.valueOf(reader.readBoolean())
        case TokenType.String =>
          reader.readString() match {
            case "true"  => java.lang.Boolean.TRUE
            case "false" => java.lang.Boolean.FALSE
            case ""      => null
            case unknown => throw new IllegalStateException(s"""Unknown boolean: "$unknown"""")
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Boolean, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeBoolean(value.booleanValue)
      }

  }

  object ScalaByteTypeAdapter extends TypeAdapter[Byte] {

    override def read(reader: Reader): Byte =
      reader.peek match {
        case TokenType.Number =>
          reader.readByte()
        case TokenType.String =>
          reader.readString().toByte
      }

    override def write(value: Byte, writer: Writer): Unit =
      writer.writeByte(value)

  }

  object JavaByteTypeAdapter extends TypeAdapter[java.lang.Byte] {

    override def read(reader: Reader): java.lang.Byte =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Byte.valueOf(reader.readByte())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Byte.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Byte, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeByte(value.byteValue)
      }

  }

  object ScalaDoubleTypeAdapter extends TypeAdapter[Double] {

    override def read(reader: Reader): Double =
      reader.peek match {
        case TokenType.Number =>
          reader.readDouble()
        case TokenType.String =>
          reader.readString().toDouble
      }

    override def write(value: Double, writer: Writer): Unit =
      writer.writeDouble(value)

  }

  object JavaDoubleTypeAdapter extends TypeAdapter[java.lang.Double] {

    override def read(reader: Reader): java.lang.Double =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Double.valueOf(reader.readDouble())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Double.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Double, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeDouble(value.doubleValue)
      }

  }

  object ScalaFloatTypeAdapter extends TypeAdapter[Float] {

    override def read(reader: Reader): Float =
      reader.peek match {
        case TokenType.Number =>
          reader.readFloat()
        case TokenType.String =>
          reader.readString().toFloat
      }

    override def write(value: Float, writer: Writer): Unit =
      writer.writeFloat(value)

  }

  object JavaFloatTypeAdapter extends TypeAdapter[java.lang.Float] {

    override def read(reader: Reader): java.lang.Float =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Float.valueOf(reader.readFloat())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Float.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Float, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeFloat(value.floatValue)
      }

  }

  object ScalaIntTypeAdapter extends TypeAdapter[Int] {

    override def read(reader: Reader): Int =
      reader.peek match {
        case TokenType.Number =>
          reader.readInt()
        case TokenType.String =>
          reader.readString().toInt
      }

    override def write(value: Int, writer: Writer): Unit =
      writer.writeInt(value)

  }

  object JavaIntegerTypeAdapter extends TypeAdapter[java.lang.Integer] {

    override def read(reader: Reader): java.lang.Integer =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Integer.valueOf(reader.readInt())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Integer.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Integer, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeInt(value.intValue)
      }

  }

  object ScalaLongTypeAdapter extends TypeAdapter[Long] {

    override def read(reader: Reader): Long =
      reader.peek match {
        case TokenType.Number =>
          reader.readLong()
        case TokenType.String =>
          reader.readString().toLong
      }

    override def write(value: Long, writer: Writer): Unit =
      writer.writeLong(value)

  }

  object JavaLongTypeAdapter extends TypeAdapter[java.lang.Long] {

    override def read(reader: Reader): java.lang.Long =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Long.valueOf(reader.readLong())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Long.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Long, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeLong(value.longValue)
      }

  }

  object ScalaShortTypeAdapter extends TypeAdapter[Short] {

    override def read(reader: Reader): Short =
      reader.peek match {
        case TokenType.Number =>
          reader.readShort()
        case TokenType.String =>
          reader.readString().toShort
      }

    override def write(value: Short, writer: Writer): Unit =
      writer.writeShort(value)

  }

  object JavaShortTypeAdapter extends TypeAdapter[java.lang.Short] {

    override def read(reader: Reader): java.lang.Short =
      reader.peek match {
        case TokenType.Number =>
          java.lang.Short.valueOf(reader.readShort())
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              java.lang.Short.valueOf(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.lang.Short, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeShort(value.shortValue)
      }

  }

  object ScalaBigIntTypeAdapter extends TypeAdapter[BigInt] {

    override def read(reader: Reader): BigInt =
      reader.peek match {
        case TokenType.Number =>
          reader.read(expected = TokenType.Number)
          BigInt(reader.tokenText)
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              BigInt(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: BigInt, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeRawValue(value.toString)
      }

  }

  object JavaBigIntegerTypeAdapter extends TypeAdapter[java.math.BigInteger] {

    override def read(reader: Reader): java.math.BigInteger =
      reader.peek match {
        case TokenType.Number =>
          reader.read(expected = TokenType.Number)
          new java.math.BigInteger(reader.tokenText)
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              new java.math.BigInteger(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.math.BigInteger, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeRawValue(value.toString)
      }

  }

  object ScalaBigDecimalTypeAdapter extends TypeAdapter[BigDecimal] {

    override def read(reader: Reader): BigDecimal =
      reader.peek match {
        case TokenType.Number =>
          reader.read(expected = TokenType.Number)
          BigDecimal(reader.tokenText)
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              BigDecimal(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: BigDecimal, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeRawValue(value.toString)
      }

  }

  object JavaBigDecimalTypeAdapter extends TypeAdapter[java.math.BigDecimal] {

    override def read(reader: Reader): java.math.BigDecimal =
      reader.peek match {
        case TokenType.Number =>
          reader.read(expected = TokenType.Number)
          new java.math.BigDecimal(reader.tokenText)
        case TokenType.String =>
          reader.readString() match {
            case "" =>
              null
            case nonEmpty =>
              new java.math.BigDecimal(nonEmpty)
          }
        case TokenType.Null =>
          reader.readNull()
      }

    override def write(value: java.math.BigDecimal, writer: Writer): Unit =
      if (value == null) {
        writer.writeNull()
      } else {
        writer.writeRawValue(value.toString)
      }

  }
*/
}
