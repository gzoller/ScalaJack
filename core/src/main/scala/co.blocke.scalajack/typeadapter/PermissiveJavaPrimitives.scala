package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._
import util.Path

object PermissiveJavaBigDecimalTypeAdapterFactory extends TypeAdapter.=:=[BigDecimal] with JavaBigDecimalTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigDecimal =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaBigIntegerTypeAdapterFactory extends TypeAdapter.=:=[BigInteger] with JavaBigIntegerTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): BigInteger =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaBooleanTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Boolean] with JavaBooleanTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Boolean =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaByteTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Byte] with JavaByteTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Byte =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaCharacterTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Character] with JavaCharacterTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Character =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaDoubleTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Double] with JavaDoubleTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Double =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaFloatTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Float] with JavaFloatTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Float =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaIntTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Integer] with JavaIntTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Integer =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaLongTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Long] with JavaLongTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Long =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaNumberTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Number] with JavaNumberTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Number =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}

object PermissiveJavaShortTypeAdapterFactory extends TypeAdapter.=:=[java.lang.Short] with JavaShortTypeAdapter {
  override def read[WIRE](path: Path, reader: Transceiver[WIRE]): java.lang.Short =
    if (reader.peek() == TokenType.String)
      (new StringWrapTypeAdapter(this)).read(path, reader)
    else
      super.read(path, reader)
}