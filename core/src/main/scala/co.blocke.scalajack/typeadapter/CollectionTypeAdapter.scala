package co.blocke.scalajack
package typeadapter

import scala.collection.generic.CanBuildFrom
import scala.collection.{ GenTraversableOnce, mutable }

object CollectionTypeAdapter extends TypeAdapterFactory.<:<.withOneTypeParam[GenTraversableOnce] {

  override def create[E, C <: GenTraversableOnce[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[C], ttTraversable: TypeTag[GenTraversableOnce[E]], ttElement: TypeTag[E]): TypeAdapter[C] = {
    val elementTypeAdapter = context.typeAdapterOf[E]

    val canBuildFromEntries = CanBuildFroms.to[C]

    canBuildFromEntries.headOption match {
      case None =>
        next.typeAdapterOf[C]

      case Some(canBuildFromEntry) =>
        val canBuildFrom = canBuildFromEntry.canBuildFrom.asInstanceOf[CanBuildFrom[_, E, C]]

        def newBuilder(): mutable.Builder[E, C] = canBuildFrom()

        val deserializer = new CollectionDeserializer[E, C](
          elementDeserializer = elementTypeAdapter.deserializer,
          newBuilder          = () => newBuilder)

        val serializer = new CollectionSerializer[E, C](
          elementSerializer = elementTypeAdapter.serializer)

        CollectionTypeAdapter[E, C](deserializer, serializer)
    }
  }

}

case class CollectionTypeAdapter[E, C <: GenTraversableOnce[E]](override val deserializer: Deserializer[C], override val serializer: Serializer[C]) extends TypeAdapter[C]
