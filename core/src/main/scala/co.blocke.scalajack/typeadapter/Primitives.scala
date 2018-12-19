package co.blocke.scalajack
package typeadapter

import model._

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read(reader: Reader, isMapKey: Boolean = false): Boolean = reader.readBoolean(isMapKey)
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read(reader: Reader, isMapKey: Boolean = false): Byte = reader.readInt(isMapKey).toByte
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read(reader: Reader, isMapKey: Boolean = false): Double = reader.readDouble(isMapKey)
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read(reader: Reader, isMapKey: Boolean = false): Float = reader.readDouble(isMapKey).toFloat
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read(reader: Reader, isMapKey: Boolean = false): Int = reader.readInt(isMapKey)
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read(reader: Reader, isMapKey: Boolean = false): Long = reader.readLong(isMapKey)
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read(reader: Reader, isMapKey: Boolean = false): Short = reader.readInt(isMapKey).toShort
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read(reader: Reader, isMapKey: Boolean = false): String = reader.readString()
}
