package co.blocke.scalajack
package typeadapter

import util.Path
import model._
import java.util.UUID

import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): BigDecimal = reader.readDecimal(path, isMapKey)
  def write(t: BigDecimal, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeDecimal(t, out)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): BigInt = reader.readBigInt(path, isMapKey)
  def write(t: BigInt, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeString(t.toString, out)
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Array[Byte] =
    reader.readString(path) match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }
  def write(t: Array[Byte], writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeString("binary here", out)
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Boolean = reader.readBoolean(path, isMapKey)
  def write(t: Boolean, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeBoolean(t, out)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Byte = reader.readInt(path, isMapKey).toByte
  def write(t: Byte, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeInt(t, out)
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Char = {
    val chars = reader.readString(path).toCharArray()
    if (chars.size == 0)
      throw new SJReadError(path, Invalid, "Tried to read a Char but empty string found")
    else
      chars(0)
  }
  def write(t: Char, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeString(t.toString, out)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Double = reader.readDouble(path, isMapKey)
  def write(t: Double, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeDouble(t, out)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Float = reader.readDouble(path, isMapKey).toFloat
  def write(t: Float, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeDouble(t, out)
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Int = reader.readInt(path, isMapKey)
  def write(t: Int, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeInt(t, out)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Long = reader.readLong(path, isMapKey)
  def write(t: Long, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeLong(t, out)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): Short = reader.readInt(path, isMapKey).toShort
  def write(t: Short, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeInt(t, out)
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): String = reader.readString(path)
  def write(t: String, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeString(t, out)
}

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] {
  def read(path: Path, reader: Transceiver, isMapKey: Boolean = false): UUID = {
    reader.readString(path) match {
      case null => null
      case s: String =>
        Try(UUID.fromString(s)) match {
          case Success(u) => u
          case Failure(u) => throw new SJReadError(path, Invalid,
            s"Failed to create UUID value from parsed text ${s}",
                                                   List(s), Some(u))
        }
    }
  }
  def write(t: UUID, writer: Transceiver)(out: Builder[Any, writer.WIRE]): Unit = writer.writeString(t.toString, out)
}
