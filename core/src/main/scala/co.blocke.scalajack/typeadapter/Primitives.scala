package co.blocke.scalajack
package typeadapter

import util.Path
import model._
import java.util.UUID
import org.apache.commons.codec.binary.Base64

import scala.util.{ Failure, Success, Try }

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): BigDecimal = reader.readDecimal(path, isMapKey)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): BigInt = reader.readBigInt(path, isMapKey)
}

object BinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Array[Byte] =
    reader.readString(path) match {
      case null      => null
      case s: String => Base64.decodeBase64(s)
    }
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Boolean = reader.readBoolean(path, isMapKey)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Byte = reader.readInt(path, isMapKey).toByte
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Char = {
    val chars = reader.readString(path).toCharArray()
    if (chars.size == 0)
      throw new SJReadError(path, Invalid, "Tried to read a Char but empty string found")
    else
      chars(0)
  }
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Double = reader.readDouble(path, isMapKey)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Float = reader.readDouble(path, isMapKey).toFloat
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Int = reader.readInt(path, isMapKey)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Long = reader.readLong(path, isMapKey)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Short = reader.readInt(path, isMapKey).toShort
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): String = reader.readString(path)
}

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): UUID = {
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
}
