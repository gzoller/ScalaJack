package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._
import util.Path

object PermissiveJavaBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with JavaBigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigDecimal =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaBigIntegerTypeAdapterFactory extends TypeAdapter.=:=[BigInteger] with JavaBigIntegerTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): BigInteger =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaBooleanTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Boolean] with JavaBooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Boolean =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaByteTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Byte] with JavaByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Byte =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaDoubleTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Double] with JavaDoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Double =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaFloatTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Float] with JavaFloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Float =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaIntTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Integer] with JavaIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Integer =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaLongTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Long] with JavaLongTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Long =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaNumberTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Number] with JavaNumberTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Number =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}

object PermissiveJavaShortTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Short] with JavaShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): java.lang.Short =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader, isMapKey)
}