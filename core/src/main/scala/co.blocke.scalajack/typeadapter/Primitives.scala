package co.blocke.scalajack
package typeadapter

import model._

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {
  def read(reader: Reader): Boolean = reader.readBoolean()
}

object ByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] {
  def read(reader: Reader): Byte = reader.readInt().toByte
}

object DoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] {
  def read(reader: Reader): Double = reader.readDouble()
}

object FloatTypeAdapterFactory extends TypeAdapter.=:=[Float] {
  def read(reader: Reader): Float = reader.readDouble().toFloat
}

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {
  def read(reader: Reader): Int = reader.readInt()
}

object LongTypeAdapterFactory extends TypeAdapter.=:=[Long] {
  def read(reader: Reader): Long = reader.readLong()
}

object ShortTypeAdapterFactory extends TypeAdapter.=:=[Short] {
  def read(reader: Reader): Short = reader.readInt().toShort
}

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {
  def read(reader: Reader): String = reader.readString()
}
