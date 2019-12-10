package co.blocke.scalajack
package typeadapter

import model._

object PermissiveBigDecimalTypeAdapterFactory
  extends TypeAdapter.=:=[BigDecimal]
  with BigDecimalTypeAdapter {
  override def read(parser: Parser): BigDecimal =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveBigIntTypeAdapterFactory
  extends TypeAdapter.=:=[BigInt]
  with BigIntTypeAdapter {
  override def read(parser: Parser): BigInt =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveBooleanTypeAdapterFactory
  extends TypeAdapter.=:=[Boolean]
  with BooleanTypeAdapter {
  override def read(parser: Parser): Boolean =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveByteTypeAdapterFactory
  extends TypeAdapter.=:=[Byte]
  with ByteTypeAdapter {
  override def read(parser: Parser): Byte =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveDoubleTypeAdapterFactory
  extends TypeAdapter.=:=[Double]
  with DoubleTypeAdapter {
  override def read(parser: Parser): Double =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveFloatTypeAdapterFactory
  extends TypeAdapter.=:=[Float]
  with FloatTypeAdapter {
  override def read(parser: Parser): Float =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveIntTypeAdapterFactory
  extends TypeAdapter.=:=[Int]
  with IntTypeAdapter {
  override def read(parser: Parser): Int =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveLongTypeAdapterFactory
  extends TypeAdapter.=:=[Long]
  with LongTypeAdapter {
  override def read(parser: Parser): Long =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}

object PermissiveShortTypeAdapterFactory
  extends TypeAdapter.=:=[Short]
  with ShortTypeAdapter {
  override def read(parser: Parser): Short =
    if (parser.nextIsString)
      parser.jackFlavor
        .stringWrapTypeAdapterFactory(this, emptyStringOk = false)
        .read(parser)
    else
      super.read(parser)
}
