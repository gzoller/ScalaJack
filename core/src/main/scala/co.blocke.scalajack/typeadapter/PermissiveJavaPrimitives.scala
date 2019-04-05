package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._
import util.Path

object PermissiveJavaBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with JavaBigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): BigDecimal =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaBigIntegerTypeAdapterFactory extends TypeAdapter.=:=[BigInteger] with JavaBigIntegerTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): BigInteger =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaBooleanTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Boolean] with JavaBooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Boolean =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaByteTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Byte] with JavaByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Byte =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaDoubleTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Double] with JavaDoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Double =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaFloatTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Float] with JavaFloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Float =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaIntTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Integer] with JavaIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Integer =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaLongTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Long] with JavaLongTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Long =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaNumberTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Number] with JavaNumberTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Number =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaShortTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Short] with JavaShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Reader[WIRE]): java.lang.Short =
    if (reader.head.tokenType == TokenType.String)
      reader.jackFlavor.stringWrapTypeAdapterFactory(this).read(path, reader)
    else
      super.read(path, reader)
}