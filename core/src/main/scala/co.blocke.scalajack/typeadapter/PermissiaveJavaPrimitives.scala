package co.blocke.scalajack
package typeadapter

import java.math.BigDecimal
import java.math.BigInteger

import model._

object PermissiveJavaBigDecimalTypeAdapterFactory
  extends TypeAdapter.=:=[BigDecimal]
  with JavaBigDecimalTypeAdapter {
  override def read(parser: Parser): BigDecimal =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaBigIntegerTypeAdapterFactory
  extends TypeAdapter.=:=[BigInteger]
  with JavaBigIntegerTypeAdapter {
  override def read(parser: Parser): BigInteger =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaBooleanTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Boolean]
  with JavaBooleanTypeAdapter {
  override def read(parser: Parser): java.lang.Boolean =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaByteTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Byte]
  with JavaByteTypeAdapter {
  override def read(parser: Parser): java.lang.Byte =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaDoubleTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Double]
  with JavaDoubleTypeAdapter {
  override def read(parser: Parser): java.lang.Double =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaFloatTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Float]
  with JavaFloatTypeAdapter {
  override def read(parser: Parser): java.lang.Float =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaIntTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Integer]
  with JavaIntTypeAdapter {
  override def read(parser: Parser): java.lang.Integer =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaLongTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Long]
  with JavaLongTypeAdapter {
  override def read(parser: Parser): java.lang.Long =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaNumberTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Number]
  with JavaNumberTypeAdapter {
  override def read(parser: Parser): java.lang.Number =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveJavaShortTypeAdapterFactory
  extends TypeAdapter.=:=[java.lang.Short]
  with JavaShortTypeAdapter {
  override def read(parser: Parser): java.lang.Short =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}
