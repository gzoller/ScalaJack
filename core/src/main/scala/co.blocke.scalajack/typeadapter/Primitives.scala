package co.blocke.scalajack
package typeadapter

import util.Path
import model._
import java.util.UUID

object BigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): BigDecimal = reader.readDecimal(isMapKey)
}

object BigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): BigInt = reader.readBigInt(isMapKey)
}

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Boolean = reader.readBoolean(isMapKey)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Byte = reader.readInt(isMapKey).toByte
}

object CharTypeAdapterFactory extends TypeAdapter.=:=[Char] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Char = reader.readString().toCharArray()(0)
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Double = reader.readDouble(isMapKey)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Float = reader.readDouble(isMapKey).toFloat
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Int = reader.readInt(isMapKey)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Long = reader.readLong(isMapKey)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): Short = reader.readInt(isMapKey).toShort
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): String = reader.readString()
}

object UUIDTypeAdapterFactory extends TypeAdapter.=:=[UUID] {
  def read(path: Path, reader: Reader, isMapKey: Boolean = false): UUID = UUID.fromString(reader.readString())
}
