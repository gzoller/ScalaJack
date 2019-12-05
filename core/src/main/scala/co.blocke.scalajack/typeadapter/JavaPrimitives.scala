package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._
import scala.collection.mutable

object JavaBigDecimalTypeAdapterFactory
  extends TypeAdapter.=:=[BigDecimal]
  with JavaBigDecimalTypeAdapter
trait JavaBigDecimalTypeAdapter {
  def read(parser: Parser): BigDecimal = parser.expectNumber() match {
    case null => null
    case bd   => new BigDecimal(bd)
  }
  def write[WIRE](
      t:      BigDecimal,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDecimal(t, out)
  }
}

object JavaBigIntegerTypeAdapterFactory
  extends TypeAdapter.=:=[BigInteger]
  with JavaBigIntegerTypeAdapter
trait JavaBigIntegerTypeAdapter {
  def read(parser: Parser): BigInteger = parser.expectNumber() match {
    case null => null
    case bi   => new BigInteger(bi)
  }
  def write[WIRE](
      t:      BigInteger,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBigInt(t, out)
  }
}

object JavaBooleanTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Boolean]
  with JavaBooleanTypeAdapter
trait JavaBooleanTypeAdapter {
  def read(parser: Parser): java.lang.Boolean =
    if (parser.peekForNull)
      null // Booleans are nullable in Java, but not in Scala
    else
      new java.lang.Boolean(parser.expectBoolean())
  def write[WIRE](
      t:      java.lang.Boolean,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBoolean(t, out)
  }
}

object JavaByteTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Byte]
  with JavaByteTypeAdapter
trait JavaByteTypeAdapter {
  def read(parser: Parser): java.lang.Byte =
    if (parser.peekForNull)
      null // Bytes are nullable in Java, but not Scala
    else
      new java.lang.Byte(parser.expectNumber().toInt.toByte)
  def write[WIRE](
      t:      java.lang.Byte,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t.toByte, out)
  }
}

object JavaCharacterTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Character]
  with JavaCharacterTypeAdapter
  with Stringish
trait JavaCharacterTypeAdapter {
  def read(parser: Parser): java.lang.Character =
    if (parser.peekForNull)
      null
    else
      parser.expectString() match {
        case "" =>
          parser.backspace()
          throw new ScalaJackError(
            parser.showError("Tried to read a Character but empty string found")
          )
        case c => new java.lang.Character(c.toCharArray()(0))
      }
  def write[WIRE](
      t:      java.lang.Character,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(t.toString, out)
  }
}

object JavaDoubleTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Double]
  with JavaDoubleTypeAdapter
trait JavaDoubleTypeAdapter {
  def read(parser: Parser): java.lang.Double =
    if (parser.peekForNull)
      null
    else
      new java.lang.Double(parser.expectNumber())
  def write[WIRE](
      t:      java.lang.Double,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDouble(t, out)
  }
}

object JavaFloatTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Float]
  with JavaFloatTypeAdapter
trait JavaFloatTypeAdapter {
  def read(parser: Parser): java.lang.Float =
    if (parser.peekForNull)
      null
    else
      new java.lang.Float(parser.expectNumber())
  def write[WIRE](
      t:      java.lang.Float,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeDouble(util.FixFloat.capFloat(t), out)
  }
}

object JavaIntTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Integer]
  with JavaIntTypeAdapter
trait JavaIntTypeAdapter {
  def read(parser: Parser): java.lang.Integer =
    if (parser.peekForNull)
      null
    else
      new java.lang.Integer(parser.expectNumber())
  def write[WIRE](
      t:      java.lang.Integer,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t, out)
  }
}

object JavaLongTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Long]
  with JavaLongTypeAdapter
trait JavaLongTypeAdapter {
  def read(parser: Parser): java.lang.Long =
    if (parser.peekForNull)
      null
    else
      new java.lang.Long(parser.expectNumber())
  def write[WIRE](
      t:      java.lang.Long,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeLong(t, out)
  }
}

object JavaNumberTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Number]
  with JavaNumberTypeAdapter
trait JavaNumberTypeAdapter {
  def read(parser: Parser): java.lang.Number =
    if (parser.peekForNull)
      null
    else
      scala.BigDecimal(parser.expectNumber()) match {
        case d if d.isValidByte     => new java.lang.Byte(d.toByteExact)
        case d if d.isValidShort    => new java.lang.Short(d.toShortExact)
        case d if d.isValidInt      => new java.lang.Integer(d.toIntExact)
        case d if d.isValidLong     => new java.lang.Long(d.toLongExact)
        case d if d.isDecimalFloat  => new java.lang.Float(d.toFloat)
        case d if d.isDecimalDouble => new java.lang.Double(d.toDouble)
        case d                      => d.bigDecimal
      }
  def write[WIRE](
      t:      java.lang.Number,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case t if t.isInstanceOf[java.lang.Integer] =>
      writer.writeInt(t.intValue, out)
    case t if t.isInstanceOf[java.lang.Long] =>
      writer.writeLong(t.longValue, out)
    case t if t.isInstanceOf[java.lang.Byte] =>
      writer.writeInt(t.byteValue, out)
    case t if t.isInstanceOf[java.lang.Short] =>
      writer.writeInt(t.shortValue, out)
    case t if t.isInstanceOf[java.lang.Float] =>
      writer.writeDouble(util.FixFloat.capFloat(t.floatValue), out)
    case t if t.isInstanceOf[java.lang.Double] =>
      writer.writeDouble(t.doubleValue, out)
    case t if t.isInstanceOf[java.math.BigInteger] =>
      writer.writeBigInt(BigInt(t.asInstanceOf[BigInteger]), out)
    case t if t.isInstanceOf[java.math.BigDecimal] =>
      writer.writeDecimal(scala.BigDecimal(t.asInstanceOf[BigDecimal]), out)
  }
}

object JavaShortTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Short]
  with JavaShortTypeAdapter
trait JavaShortTypeAdapter {
  def read(parser: Parser): java.lang.Short =
    if (parser.peekForNull)
      null
    else
      new java.lang.Short(parser.expectNumber())
  def write[WIRE](
      t:      java.lang.Short,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeInt(t.intValue, out)
  }
}
