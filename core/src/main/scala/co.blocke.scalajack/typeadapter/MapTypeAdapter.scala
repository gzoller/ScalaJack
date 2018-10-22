package co.blocke.scalajack
package typeadapter

import scala.collection.GenMap
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

object MapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[GenMap] {

  override def create[K, V, M <: GenMap[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[GenMap[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {
    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    CanBuildFroms.to[M].headOption match {
      case None =>
        next.typeAdapterOf[M]

      case Some(canBuildFromEntry) =>
        val canBuildFrom = canBuildFromEntry.canBuildFrom.asInstanceOf[CanBuildFrom[_, (K, V), M]]

        def newBuilder(): mutable.Builder[(K, V), M] = canBuildFrom()

        val mapDeserializer = new MapDeserializer[K, V, M](
          keyDeserializer   = keyTypeAdapter.deserializer,
          valueDeserializer = valueTypeAdapter.deserializer,
          null,
          () => newBuilder)

        val mapSerializer = new MapSerializer[K, V, M](
          keySerializer   = keyTypeAdapter.serializer,
          valueSerializer = valueTypeAdapter.serializer,
          context)

        MapTypeAdapter[K, V, M](mapDeserializer, mapSerializer)
    }
  }

}

case class MapTypeAdapter[K, V, M <: GenMap[K, V]](override val deserializer: Deserializer[M], override val serializer: Serializer[M]) extends TypeAdapter[M]
