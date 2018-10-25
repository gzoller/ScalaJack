package co.blocke.scalajack
package typeadapter

object AstParsingFallbackTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val nextTypeAdapter = next.typeAdapterOf[T]

    new AstParsingFallbackTypeAdapter[T](new AstParsingFallbackDeserializer[T](nextTypeAdapter.deserializer), nextTypeAdapter.serializer, nextTypeAdapter)
  }

}

class AstParsingFallbackTypeAdapter[T](override val deserializer: Deserializer[T], override val serializer: Serializer[T], override val decorated: TypeAdapter[T]) extends DecoratingTypeAdapter[T]
