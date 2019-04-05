package co.blocke.scalajack
package typeadapter

import util.Path
import model._

object PermissiveBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): BigDecimal =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveBigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): BigInt =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveBooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Boolean =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Byte =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveDoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Double =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveFloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Float =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveIntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Int =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveLongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Long =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): Short =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}
