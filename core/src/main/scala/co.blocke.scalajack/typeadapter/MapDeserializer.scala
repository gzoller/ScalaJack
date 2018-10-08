package co.blocke.scalajack
package typeadapter

import scala.collection.{ GenMap, immutable, mutable }
import scala.util.control.NonFatal

class MapDeserializer[K, V, M <: GenMap[K, V]](
    keyDeserializer:           Deserializer[K],
    valueDeserializer:         Deserializer[V],
    keyValuePairsDeserializer: Deserializer[List[(K, V)]],
    newBuilder:                () => mutable.Builder[(K, V), M])(implicit tt: TypeTag[M], ttk: TypeTag[K], ttv: TypeTag[V], context: Context) extends Deserializer[M] {

  self =>

  private val taggedNull: TypeTagged[M] = TypeTagged(null.asInstanceOf[M], tt.tpe)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[M] = {
    json match {
      case JsonNull() =>
        DeserializationSuccess(taggedNull)

      case JsonObject(x) =>
        println("Map Json: " + json)
        try {
          val objectFields = x.asInstanceOf[ops.ObjectFields]

          val builder = newBuilder()
          val taggedKeysBuilder = List.newBuilder[TypeTagged[K]]
          val taggedValuesBuilder = List.newBuilder[TypeTagged[V]]

          val errorsBuilder = immutable.Seq.newBuilder[(Path, DeserializationError)]

          ops.foreachObjectField(objectFields, { (fieldName, fieldValueJson) =>
            val keyDeserializationResult = keyDeserializer.deserialize(path \ fieldName, JsonString[J](fieldName))(ops, guidance.withMapKey())
            val valueDeserializationResult = valueDeserializer.deserialize(path \ fieldName, fieldValueJson)(ops, guidance.withMapValue())

            //            println("K: " + keyDeserializationResult)
            //            println("V: " + valueDeserializationResult)
            //            println("--------------")
            (keyDeserializationResult, valueDeserializationResult) match {
              case (DeserializationSuccess(taggedKey), DeserializationSuccess(taggedValue)) =>
                val TypeTagged(key) = taggedKey
                taggedKeysBuilder += taggedKey

                val TypeTagged(value) = taggedValue
                taggedValuesBuilder += taggedValue

                builder += key -> value

              case _ =>
                errorsBuilder ++= keyDeserializationResult.errors
                errorsBuilder ++= valueDeserializationResult.errors
            }
          })

          val errors = errorsBuilder.result()

          if (errors.nonEmpty) {
            DeserializationFailure(errors)
          } else {
            val map = builder.result()

            class TaggedMapFromJsonObject(override val get: M, taggedKeys: List[TypeTagged[K]], taggedValues: List[TypeTagged[V]]) extends TypeTagged[M] {
              override lazy val tpe: Type = tt.tpe
            }

            DeserializationSuccess(new TaggedMapFromJsonObject(map, taggedKeysBuilder.result(), taggedValuesBuilder.result()))
          }
        } catch {
          case NonFatal(e) =>
            DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
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

      case JsonString(s) => // Parse and deserialize non-string Map key (embedded in a string, e.g. Map as a key to another Map)
        val deserializer = context.typeAdapterOf[M].deserializer
        deserializer.deserialize(Path.Root, JsonParser.parse(s).get)

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object", reportedBy = self))
    }
  }

}
