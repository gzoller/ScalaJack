package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.Builder

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter
trait BigDecimalTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): BigDecimal = reader.readDecimal(path)
  def write[WIRE](t: BigDecimal, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeDecimal(t, out)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter
trait BigIntTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): BigInt = reader.readBigInt(path)
  def write[WIRE](t: BigInt, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeBigInt(t, out)
  }
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] with BinaryTypeAdapter with Stringish
trait BinaryTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Array[Byte] =
    reader.readString(path) match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }
  def write[WIRE](t: Array[Byte], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeString(Base64.encodeBase64String(t), out)
  }
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter
trait BooleanTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Boolean = reader.readBoolean(path)
  def write[WIRE](t: Boolean, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeBoolean(t, out)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter
trait ByteTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Byte = reader.readInt(path).toByte
  def write[WIRE](t: Byte, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] with CharTypeAdapter with Stringish
trait CharTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Char =
    reader.readString(path) match {
      // Needed because String can be null, but Char can't
      case null =>
        reader.back
        throw new ReadInvalidError(reader.showError(path, "A Char typed value cannot be null"))
      case c if c != "" =>
        c.charAt(0)
      case _ =>
        reader.back
        throw new ReadInvalidError(reader.showError(path, "Tried to read a Char but empty string found"))
    }
  def write[WIRE](t: Char, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeString(t.toString, out)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter
trait DoubleTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Double = reader.readDouble(path)
  def write[WIRE](t: Double, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeDouble(t, out)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter
trait FloatTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Float = reader.readDouble(path).toFloat
  def write[WIRE](t: Float, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeDouble(util.FixFloat.capFloat(t), out)
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter
trait IntTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Int = reader.readInt(path)
  def write[WIRE](t: Int, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter
trait LongTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Long = reader.readLong(path)
  def write[WIRE](t: Long, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeLong(t, out)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter
trait ShortTypeAdapter {
  def read[WIRE](path: Path, reader: Reader[WIRE]): Short = reader.readInt(path).toShort
  def write[WIRE](t: Short, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeInt(t, out)
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] with Stringish {
  def read[WIRE](path: Path, reader: Reader[WIRE]): String = reader.readString(path)
  def write[WIRE](t: String, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = writer.writeString(t, out)
}

