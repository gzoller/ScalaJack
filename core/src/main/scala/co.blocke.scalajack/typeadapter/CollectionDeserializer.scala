package co.blocke.scalajack
package typeadapter

import scala.collection.{ GenTraversableOnce, mutable }

class CollectionDeserializer[E, C <: GenTraversableOnce[E]](elementDeserializer: Deserializer[E], newBuilder: () => mutable.Builder[E, C])(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private val taggedNull: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], tt.tpe)

  private class TaggedCollection(override val get: C, taggedElements: List[TypeTagged[E]]) extends TypeTagged[C] {
    override lazy val tpe: Type = {
      //      val elementType = lub(taggedElements.map(_.tpe))
      typeOf[C]
    }
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(taggedNull)

      case JsonArray(x) =>
        val arrayElements = x.asInstanceOf[ops.ArrayElements]

        val elementsBuilder = newBuilder()
        val taggedElementsBuilder = List.newBuilder[TypeTagged[E]]
        val errorSequencesBuilder = Seq.newBuilder[Seq[(Path, DeserializationError)]]

        ops.foreachArrayElement(arrayElements, { (index, elementJson) =>
          elementDeserializer.deserialize(path \ index, elementJson) match {
            case DeserializationSuccess(taggedElement @ TypeTagged(element)) =>
              elementsBuilder += element
              taggedElementsBuilder += taggedElement

            case DeserializationFailure(errorSequence) =>
              errorSequencesBuilder += errorSequence
          }
        })

        val errorSequences: Seq[Seq[(Path, DeserializationError)]] = errorSequencesBuilder.result()
        if (errorSequences.isEmpty) {
          val taggedElements = taggedElementsBuilder.result()
          DeserializationSuccess(new TaggedCollection(elementsBuilder.result(), taggedElements))
        } else {
          DeserializationFailure(errorSequences.flatten.to[collection.immutable.Seq])
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a JSON array, not $json", reportedBy = self))
    }

}
