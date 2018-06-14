package co.blocke.scalajack
package typeadapter

import scala.collection.GenMap

class MapSerializer[K, V, M <: GenMap[K, V]](keySerializer: Serializer[K], valueSerializer: Serializer[V]) extends Serializer[M] {

  override def serialize[J](tagged: TypeTagged[M])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(map) =>
        SerializationSuccess(JsonObject { appendField =>
          lazy val mapType: Type = tagged.tpe.baseType(symbolOf[GenMap[_, _]])

          lazy val keyType: Type = {
            val k :: _ :: Nil = mapType.typeArgs
            k
          }

          lazy val valueType: Type = {
            val _ :: v :: Nil = mapType.typeArgs
            v
          }

          class TaggedKey(override val get: K) extends TypeTagged[K] {
            override def tpe: Type = keyType
          }

          class TaggedValue(override val get: V) extends TypeTagged[V] {
            override def tpe: Type = valueType
          }

          map foreach {
            case (key, value) =>
              val SerializationSuccess(keyJson) = keySerializer.serialize(new TaggedKey(key))
              val SerializationSuccess(valueJson) = valueSerializer.serialize(new TaggedValue(value))

              val keyString =
                keyJson match {
                  case JsonString(s) => s
                  case _             => ops.renderCompact(keyJson)
                }

              appendField(keyString, valueJson)
          }
        })

    }

}
