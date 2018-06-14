package co.blocke.scalajack
package typeadapter

import scala.collection.{ GenTraversableOnce, mutable }
import scala.reflect.runtime.universe.lub

class CollectionDeserializer[E, C <: GenTraversableOnce[E]](elementDeserializer: Deserializer[E], newBuilder: () => mutable.Builder[E, C])(implicit tt: TypeTag[C]) extends Deserializer[C] {

  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], tt.tpe)

  private class TaggedCollection(override val get: C, taggedElements: List[TypeTagged[E]]) extends TypeTagged[C] {
    override lazy val tpe: Type = {
      val elementType = lub(taggedElements.map(_.tpe))
      ???
    }
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonArray(x) =>
        DeserializationResult(path) {
          val arrayElements = x.asInstanceOf[ops.ArrayElements]

          val builder = newBuilder()
          val taggedElementsBuilder = List.newBuilder[TypeTagged[E]]

          ops.foreachArrayElement(arrayElements, { (index, elementJson) =>
            val DeserializationSuccess(taggedElement) = elementDeserializer.deserialize(path \ index, elementJson)
            val TypeTagged(element) = taggedElement

            builder += element
            taggedElementsBuilder += taggedElement
          })

          val taggedElements = taggedElementsBuilder.result()

          new TaggedCollection(builder.result(), taggedElements)
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON array"))
    }

}
