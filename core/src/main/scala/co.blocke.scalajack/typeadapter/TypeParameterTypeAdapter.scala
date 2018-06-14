package co.blocke.scalajack
package typeadapter

object TypeParameterTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter) {
      val anyTypeAdapter = context.typeAdapterOf[Any]
      TypeParameterTypeAdapter[T](anyTypeAdapter.deserializer.asInstanceOf[Deserializer[T]], anyTypeAdapter.serializer.asInstanceOf[Serializer[T]], anyTypeAdapter)
    } else {
      next.typeAdapterOf[T]
    }

}

case class TypeParameterTypeAdapter[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T], anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    anyTypeAdapter.read(reader).asInstanceOf[T]

  override def write(value: T, writer: Writer): Unit =
    anyTypeAdapter.write(value, writer)

}
