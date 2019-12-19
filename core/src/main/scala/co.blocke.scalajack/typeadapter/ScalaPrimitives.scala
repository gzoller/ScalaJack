package co.blocke.scalajack
package typeadapter

import org.apache.commons.codec.binary.Base64
import model._

import scala.collection.mutable

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter
trait BigDecimalTypeAdapter {
  def read(parser: Parser): BigDecimal = {
    val bd = parser.expectNumber(true)
    if (bd == null)
      null
    else
      BigDecimal(bd)
  }
  def write[WIRE](t: BigDecimal, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeDecimal(t, out)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter
trait BigIntTypeAdapter {
  def read(parser: Parser): BigInt = {
    val bi = parser.expectNumber(true)
    if (bi == null)
      null
    else
      BigInt(bi)
  }
  def write[WIRE](t: BigInt, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBigInt(t, out)
  }
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] with BinaryTypeAdapter with Stringish
trait BinaryTypeAdapter {
  def read(parser: Parser): Array[Byte] =
    parser.expectString() match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }

  def write[WIRE](t: Array[Byte], writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    => writer.writeString(Base64.encodeBase64String(t), out)
    }
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter
trait BooleanTypeAdapter {
  def read(parser: Parser): Boolean = parser.expectBoolean()
  def write[WIRE](t: Boolean, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeBoolean(t, out)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter
trait ByteTypeAdapter {
  def read(parser: Parser): Byte =
    parser
      .expectNumber()
      .toByteOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Byte from value")
        )
      }
  def write[WIRE](t: Byte, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeInt(t, out)
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] with CharTypeAdapter with Stringish
trait CharTypeAdapter {
  def read(parser: Parser): Char =
    parser.expectString() match {
      case null =>
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("A Char typed value cannot be null")
        )
      case "" =>
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Tried to read a Char but empty string found")
        )
      case s => s.charAt(0)
    }
  def write[WIRE](t: Char, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeString(t.toString, out)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter
trait DoubleTypeAdapter {
  def read(parser: Parser): Double =
    parser
      .expectNumber()
      .toDoubleOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Double from value")
        )
      }
  def write[WIRE](t: Double, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeDouble(t, out)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter
trait FloatTypeAdapter {
  //  def read(parser: Parser): Float = parser.expectNumber().toFloat
  def read(parser: Parser): Float =
    parser
      .expectNumber()
      .toFloatOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Float from value")
        )
      }
  def write[WIRE](t: Float, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeDouble(util.FixFloat.capFloat(t), out)
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter
trait IntTypeAdapter {
  //  def read(parser: Parser): Int = parser.expectNumber().toInt
  def read(parser: Parser): Int =
    parser
      .expectNumber()
      .toIntOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Int from value")
        )
      }
  def write[WIRE](t: Int, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeInt(t, out)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter
trait LongTypeAdapter {
  //  def read(parser: Parser): Long = parser.expectNumber().toLong
  def read(parser: Parser): Long =
    parser
      .expectNumber()
      .toLongOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Long from value")
        )
      }
  def write[WIRE](t: Long, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeLong(t, out)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter
trait ShortTypeAdapter {
  //  def read(parser: Parser): Short = parser.expectNumber().toInt.toShort
  def read(parser: Parser): Short =
    parser
      .expectNumber()
      .toShortOption
      .getOrElse {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError("Cannot parse an Short from value")
        )
      }
  def write[WIRE](t: Short, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeInt(t, out)
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] with Stringish {
  def read(parser: Parser): String = parser.expectString()
  def write[WIRE](t: String, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    writer.writeString(t, out)
}
