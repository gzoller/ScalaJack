package co.blocke.scalajack
package typeadapter
package javacollections

class JavaCollectionDeserializer[E, C <: java.util.Collection[E]](elementDeserializer: Deserializer[E], newEmptyCollection: () => C)(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private val collectionType: Type = tt.tpe
  private val collectionTypeConstructor: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], collectionType)

  private class TaggedCollection(override val get: C, taggedElements: List[TypeTagged[E]]) extends TypeTagged[C] {
    override lazy val tpe: Type = appliedType(collectionTypeConstructor, taggedElements.map(_.tpe)) // FIXME `C` may not actually have a type parameter.
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[C] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonArray(x) =>
        val elementsJson = x.asInstanceOf[ops.ArrayElements]

        DeserializationResult(path) {
          val collection: C = newEmptyCollection()

          val taggedElementsBuilder = List.newBuilder[TypeTagged[E]]

          ops.foreachArrayElement(elementsJson, { (index, elementJson) =>
            val DeserializationSuccess(taggedElement) = elementDeserializer.deserialize(path \ index, elementJson)
            val TypeTagged(element) = taggedElement
            taggedElementsBuilder += taggedElement
            collection.add(element)
          })

          val taggedElements = taggedElementsBuilder.result()

          new TaggedCollection(collection, taggedElements)
        }

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported(s"Expected a JSON array, not $json", reportedBy = self))
    }

}
