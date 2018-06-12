package co.blocke.scalajack
package typeadapter

import scala.collection.{ GenMap, GenMapLike, mutable }

class MapDeserializer[K, V, M <: GenMap[K, V] with GenMapLike[K, V, M]](
    keyDeserializer:           Deserializer[K],
    valueDeserializer:         Deserializer[V],
    keyValuePairsDeserializer: Deserializer[List[(K, V)]],
    newBuilder:                () => mutable.MapBuilder[K, V, M])(implicit tt: TypeTag[M]) extends Deserializer[M] {

  private val nullTypeTagged: TypeTagged[M] = TypeTagged(null.asInstanceOf[M], tt.tpe)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[M] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        DeserializationResult(path) {
          val objectFields = x.asInstanceOf[ops.ObjectFields]

          val builder = newBuilder()
          val taggedKeysBuilder = List.newBuilder[TypeTagged[K]]
          val taggedValuesBuilder = List.newBuilder[TypeTagged[V]]

          ops.foreachObjectField(objectFields, { (fieldName, fieldValueJson) =>
            val DeserializationSuccess(taggedKey) = keyDeserializer.deserialize(path \ fieldName, JsonString[J](fieldName))
            val TypeTagged(key) = taggedKey
            taggedKeysBuilder += taggedKey

            val DeserializationSuccess(taggedValue) = valueDeserializer.deserialize(path \ fieldName, fieldValueJson)
            val TypeTagged(value) = taggedValue
            taggedValuesBuilder += taggedValue

            builder += key -> value
          })

          val map = builder.result()

          class TaggedMapFromJsonObject(override val get: M, taggedKeys: List[TypeTagged[K]], taggedValues: List[TypeTagged[V]]) extends TypeTagged[M] {
            override lazy val tpe: Type = ???
          }

          new TaggedMapFromJsonObject(map, taggedKeysBuilder.result(), taggedValuesBuilder.result())
        }

      case JsonArray(_) =>
        DeserializationResult(path) {
          val DeserializationSuccess(taggedKeyValuePairs) = keyValuePairsDeserializer.deserialize(path, json)
          val TypeTagged(keyValuePairs) = taggedKeyValuePairs

          lazy val keyValuePairType: Type = taggedKeyValuePairs.tpe.baseType(symbolOf[List[_]]).typeArgs.head

          lazy val keyType: Type = {
            val k :: _ :: Nil = keyValuePairType.baseType(symbolOf[(_, _)]).typeArgs
            k
          }

          lazy val valueType: Type = {
            val _ :: v :: Nil = keyValuePairType.baseType(symbolOf[(_, _)]).typeArgs
            v
          }

          class TaggedMapFromJsonArray(override val get: M) extends TypeTagged[M] {
            override lazy val tpe: Type = appliedType(tt.tpe.typeConstructor, keyType, valueType) // TODO `M` may not actually have type parameters
          }

          val builder = newBuilder()
          builder ++= keyValuePairs
          val map = builder.result()

          new TaggedMapFromJsonArray(map)
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object"))
    }

}
