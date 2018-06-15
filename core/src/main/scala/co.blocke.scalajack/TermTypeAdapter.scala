package co.blocke.scalajack

object TermTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val ta = next.typeAdapterOf[T]
    TypeAdapter(new TermDeserializer[T](ta.deserializer), new TermSerializer(ta.serializer))
  }

}
