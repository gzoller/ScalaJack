package co.blocke.series60
package typeadapter

import util.Path
import model._

object PermissiveBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with BigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigDecimal =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveBigIntTypeAdapterFactory extends TypeAdapter.=:=[BigInt] with BigIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigInt =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveBooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] with BooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Boolean =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveByteTypeAdapterFactory extends TypeAdapter.=:=[Byte] with ByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Byte =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveDoubleTypeAdapterFactory extends TypeAdapter.=:=[Double] with DoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Double =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveFloatTypeAdapterFactory extends TypeAdapter.=:=[Float] with FloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Float =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveIntTypeAdapterFactory extends TypeAdapter.=:=[Int] with IntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Int =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveLongTypeAdapterFactory extends TypeAdapter.=:=[Long] with LongTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Long =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveShortTypeAdapterFactory extends TypeAdapter.=:=[Short] with ShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Short =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}
