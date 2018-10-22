package co.blocke.scalajack
package typeadapter

import scala.util.Try

object TryTypeAdapter extends TypeAdapterFactory.=:=.withOneTypeParam[Try] {

  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Try[E]], ttElement: TypeTag[E]): TypeAdapter[Try[E]] = {
    val valueTypeAdapter = context.typeAdapterOf[E]
    TryTypeAdapter(
      new TryDeserializer[E](valueTypeAdapter.deserializer),
      new TrySerializer[E](valueTypeAdapter.serializer),
      valueTypeAdapter)
  }

}

case class TryTypeAdapter[T](override val deserializer: Deserializer[Try[T]], override val serializer: Serializer[Try[T]], valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]]
