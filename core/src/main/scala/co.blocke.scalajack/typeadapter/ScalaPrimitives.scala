package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.Builder

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter
trait BigDecimalTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigDecimal = reader.readDecimal(path)
  def write[WIRE](t: BigDecimal, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeDecimal(t, out)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter
trait BigIntTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigInt = reader.readBigInt(path)
  def write[WIRE](t: BigInt, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeRawString(t.toString, out)
  }
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] with BinaryTypeAdapter with Stringish
trait BinaryTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Array[Byte] =
    reader.readString(path) match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }
  def write[WIRE](t: Array[Byte], writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(Base64.encodeBase64String(t), out)
  }
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter
trait BooleanTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Boolean = reader.readBoolean(path)
  def write[WIRE](t: Boolean, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeBoolean(t, out)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter
trait ByteTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Byte = reader.readInt(path).toByte
  def write[WIRE](t: Byte, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] with CharTypeAdapter with Stringish
trait CharTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Char =
    reader.readString(path) match {
      case null         => throw new ReadInvalidError(path, "A Char typed value cannot be null", List("Null"))
      case c if c != "" => c.toCharArray()(0)
      case _            => throw new ReadInvalidError(path, "Tried to read a Char but empty string found", List("Empty String"))
    }
  def write[WIRE](t: Char, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeString(t.toString, out)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter
trait DoubleTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Double = reader.readDouble(path)
  def write[WIRE](t: Double, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeDouble(t, out)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter
trait FloatTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Float = reader.readDouble(path).toFloat
  def write[WIRE](t: Float, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeDouble(util.FixFloat.capFloat(t), out)
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter
trait IntTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Int = reader.readInt(path)
  def write[WIRE](t: Int, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter
trait LongTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Long = reader.readLong(path)
  def write[WIRE](t: Long, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeLong(t, out)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter
trait ShortTypeAdapter {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): Short = reader.readInt(path).toShort
  def write[WIRE](t: Short, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] with Stringish {
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): String = reader.readString(path)
  def write[WIRE](t: String, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit = writer.writeString(t, out)
}

