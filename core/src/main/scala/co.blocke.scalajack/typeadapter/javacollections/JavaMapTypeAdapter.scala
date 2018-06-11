package co.blocke.scalajack.typeadapter
package javacollections

import co.blocke.scalajack.{ Context, DeserializationResult, DeserializationSuccess, Deserializer, JsonNull, JsonObject, JsonOps, JsonString, Path, Reader, SerializationResult, SerializationSuccess, Serializer, TypeAdapter, TypeAdapterFactory, TypeTagged, Writer }

import scala.reflect.runtime.universe.{ Symbol, Type, TypeTag, symbolOf }

object JavaMapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[java.util.Map] {

  override def create[K, V, M <: java.util.Map[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[java.util.Map[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {

    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    new JavaMapTypeAdapter[K, V, M](
      deserializer = new JavaMapDeserializer[K, V, M](keyTypeAdapter.deserializer, valueTypeAdapter.deserializer, () => ???),
      serializer   = new JavaMapSerializer[K, V, M](keyTypeAdapter.serializer, valueTypeAdapter.serializer))
  }

}

class JavaMapTypeAdapter[K, V, M <: java.util.Map[K, V]](override val deserializer: Deserializer[M], override val serializer: Serializer[M]) extends TypeAdapter[M] {

  override def read(reader: Reader): M = ???

  override def write(value: M, writer: Writer): Unit = ???

}

class JavaMapDeserializer[K, V, M <: java.util.Map[K, V]](keyDeserializer: Deserializer[K], valueDeserializer: Deserializer[V], newEmptyMap: () => M)(implicit tt: TypeTag[M]) extends Deserializer[M] {

  private class TaggedMap(override val get: M, taggedKeys: List[TypeTagged[K]], taggedValues: List[TypeTagged[V]]) extends TypeTagged[M] {
    override lazy val tpe: Type = ???
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[M] =
    json match {
      case JsonObject(x) =>
        val objectFields = x.asInstanceOf[ops.ObjectFields]

        val map: M = newEmptyMap()

        ops.foreachObjectField(objectFields, { (fieldName, fieldValue) =>
          val DeserializationSuccess(TypeTagged(key)) = keyDeserializer.deserialize(path \ fieldName, JsonString(fieldName))
          ???
        })

        ???
    }

}

class JavaMapSerializer[K, V, M <: java.util.Map[K, V]](keySerializer: Serializer[K], valueSerializer: Serializer[V]) extends Serializer[M] {

  private val mapSymbol: Symbol = symbolOf[java.util.Map[_, _]]

  override def serialize[J](tagged: TypeTagged[M])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(map) =>
        lazy val baseType: Type = tagged.tpe.baseType(mapSymbol)

        lazy val keyType: Type = {
          val k :: _ :: Nil = baseType.typeArgs
          k
        }

        lazy val valueType: Type = {
          val _ :: v :: Nil = baseType.typeArgs
          v
        }

        class TaggedKey(override val get: K) extends TypeTagged[K] {
          override def tpe: Type = keyType
        }

        class TaggedValue(override val get: V) extends TypeTagged[V] {
          override def tpe: Type = valueType
        }

        SerializationSuccess(JsonObject { appendField =>
          val iterator = map.entrySet().iterator()
          while (iterator.hasNext) {
            val mapEntry = iterator.next()
            val SerializationSuccess(JsonString(keyString)) = keySerializer.serialize(new TaggedKey(mapEntry.getKey))
            val SerializationSuccess(valueJson) = valueSerializer.serialize(new TaggedValue(mapEntry.getValue))
            appendField(keyString, valueJson)
          }
        })
    }

}
