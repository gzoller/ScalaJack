package co.blocke.scalajack
package typeadapter

import scala.collection.GenMap
import scala.collection.immutable

class MapSerializer[K, V, M <: GenMap[K, V]](keySerializer: Serializer[K], valueSerializer: Serializer[V], context: Context) extends Serializer[M] {

  override def serialize[J](tagged: TypeTagged[M])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(map) =>
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

        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val json = JsonObject[J] { appendField =>
          map foreach {
            case (key, value) =>
              val keySerializationResult = keySerializer.serialize(new TaggedKey(key))(ops, guidance.withMapKey())
              val valueSerializationResult = valueSerializer.serialize(new TaggedValue(value))(ops, guidance.withMapValue())

              (keySerializationResult, valueSerializationResult) match {
                case (SerializationSuccess(keyJson), SerializationSuccess(valueJson)) =>
                  val keyString =
                    keyJson match {
                      case JsonString(s)               => s
                      case _ if (guidance.isCanonical) => ops.renderCompact(keyJson, context.sjFlavor.get)
                      case _                           => No_Quote_Marker + ops.renderCompact(keyJson, context.sjFlavor.get)
                    }
                  appendField(keyString, valueJson)

                case (SerializationSuccess(keyJson), SerializationFailure(Seq(SerializationError.Nothing))) =>
                  val keyString =
                    keyJson match {
                      case JsonString(s) => s
                      case _             => ops.renderCompact(keyJson, context.sjFlavor.get)
                    }
                  appendField(keyString, JsonNull())

                case _ =>
                  errorsBuilder ++= keySerializationResult.errors
                  errorsBuilder ++= valueSerializationResult.errors
              }
          }
        }

        val errors = errorsBuilder.result()

        if (errors.nonEmpty) {
          SerializationFailure(errors)
        } else {
          SerializationSuccess(json)
        }

    }

}
