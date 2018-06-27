package co.blocke.scalajack
package typeadapter

object JsonParsingFallbackTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val nextTypeAdapter = next.typeAdapterOf[T]

    new JsonParsingFallbackTypeAdapter[T](new JsonParsingFallbackDeserializer[T](nextTypeAdapter.deserializer), nextTypeAdapter.serializer, nextTypeAdapter)
  }

}

class JsonParsingFallbackTypeAdapter[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T], override val decorated: TypeAdapter[T]) extends DecoratingTypeAdapter[T]
