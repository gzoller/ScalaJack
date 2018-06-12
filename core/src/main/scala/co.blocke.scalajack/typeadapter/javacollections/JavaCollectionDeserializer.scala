package co.blocke.scalajack
package typeadapter
package javacollections

import scala.reflect.runtime.universe.{ Type, TypeTag }

class JavaCollectionDeserializer[E, C <: java.util.Collection[E]](elementDeserializer: Deserializer[E], newEmptyCollection: () => C)(implicit tt: TypeTag[C]) extends Deserializer[C] {

  private val collectionType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], collectionType)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonArray(x) =>
        val elementsJson = x.asInstanceOf[ops.ArrayElements]

        DeserializationResult(path)({
          val collection: C = newEmptyCollection()

          ops.foreachArrayElement(elementsJson, { (index, elementJson) =>
            val DeserializationSuccess(TypeTagged(element)) = elementDeserializer.deserialize(path \ index, elementJson)
            collection.add(element)
          })

          TypeTagged(collection, collectionType)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON array"))
    }

}
