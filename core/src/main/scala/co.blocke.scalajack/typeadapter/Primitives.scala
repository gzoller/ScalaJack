package co.blocke.scalajack
package typeadapter

import util.Path
import model._
import java.util.UUID

import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): BigDecimal = reader.readDecimal(path, isMapKey)
  def write[WIRE](t: BigDecimal, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeDecimal(t, out)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): BigInt = reader.readBigInt(path, isMapKey)
  def write[WIRE](t: BigInt, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _    => writer.writeRawString(t.toString, out)
  }
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Array[Byte] =
    reader.readString(path) match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }
  def write[WIRE](t: Array[Byte], writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeString("binary here", out)
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Boolean = reader.readBoolean(path, isMapKey)
  def write[WIRE](t: Boolean, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeBoolean(t, out)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Byte = reader.readInt(path, isMapKey).toByte
  def write[WIRE](t: Byte, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeInt(t, out)
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Char =
    reader.readString(path) match {
      case null         => throw new ReadInvalidError(path, "A Char typed value cannot be null", List("Null"))
      case c if c != "" => c.toCharArray()(0)
      case _            => throw new ReadInvalidError(path, "Tried to read a Char but empty string found", List("Empty String"))
    }
  def write[WIRE](t: Char, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeString(t.toString, out)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Double = reader.readDouble(path, isMapKey)
  def write[WIRE](t: Double, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeDouble(t, out)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Float = reader.readDouble(path, isMapKey).toFloat
  def write[WIRE](t: Float, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeDouble(t, out)
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Int = reader.readInt(path, isMapKey)
  def write[WIRE](t: Int, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeInt(t, out)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Long = reader.readLong(path, isMapKey)
  def write[WIRE](t: Long, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeLong(t, out)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): Short = reader.readInt(path, isMapKey).toShort
  def write[WIRE](t: Short, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeInt(t, out)
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): String = reader.readString(path)
  def write[WIRE](t: String, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeString(t, out)
}

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] {
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): UUID = {
    reader.readString(path) match {
      case null => null
      case s: String =>
        Try(UUID.fromString(s)) match {
          case Success(u) => u
          case Failure(u) => throw new ReadMalformedError(
            path,
            s"Failed to create UUID value from parsed text ${s}", List(s), u)
        }
    }
  }
  def write[WIRE](t: UUID, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = writer.writeString(t.toString, out)
}
