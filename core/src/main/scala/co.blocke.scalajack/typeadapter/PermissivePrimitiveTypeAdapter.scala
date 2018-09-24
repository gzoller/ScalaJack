package co.blocke.scalajack
package typeadapter

import javaprimitives._

object PermissivePrimitiveTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    if (tt.tpe =:= typeOf[Boolean]) {
      BooleanTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Boolean]) {
      JavaBooleanTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Byte]) {
      ByteTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Byte]) {
      JavaByteTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Double]) {
      DoubleTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Double]) {
      JavaDoubleTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Float]) {
      FloatTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Float]) {
      JavaFloatTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Int]) {
      IntTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Integer]) {
      JavaIntegerTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Long]) {
      LongTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Long]) {
      JavaLongTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[Short]) {
      ShortTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.lang.Short]) {
      JavaShortTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[BigInt]) {
      BigIntTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.math.BigInteger]) {
      JavaBigIntegerTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[BigDecimal]) {
      BigDecimalTypeAdapter.asInstanceOf[TypeAdapter[T]]
    } else if (tt.tpe =:= typeOf[java.math.BigDecimal]) {
      JavaBigDecimalTypeAdapter.create(TypeAdapterFactory(Nil)).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }
  }
}
