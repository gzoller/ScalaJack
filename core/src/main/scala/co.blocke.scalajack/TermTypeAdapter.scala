package co.blocke.scalajack

object TermTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val ta = next.typeAdapterOf[T]
    val deserializer = new TermDeserializer[T](ta.deserializer)
    val serializer = new TermSerializer(ta.serializer)
    TypeAdapter(deserializer, serializer)
  }

}
