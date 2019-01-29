package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import org.apache.commons.codec.binary.Base64

import scala.collection.mutable.Builder

object PermissiveBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigDecimal =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveBigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigInt =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveBinaryTypeAdapterFactory extends TypeAdapter.=:=[Array[Byte]] with BinaryTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Array[Byte] =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveBooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Boolean =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Byte =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveCharTypeAdapterFactory extends TypeAdapter.=:=[Char] with CharTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Char =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveDoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Double =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveFloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Float =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveIntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Int =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveLongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Long =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): Short =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}
