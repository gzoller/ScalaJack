package co.blocke.scalajack
package typeadapter
package javacollections

class JavaCollectionSerializer[E, G <: java.util.Collection[E]](elementSerializer: Serializer[E]) extends Serializer[G] {

  override def serialize[J](taggedCollection: TypeTagged[G])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    taggedCollection match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(collection) =>
        lazy val elementType: Type = {
          val e :: Nil = taggedCollection.tpe.baseType(symbolOf[java.util.Collection[_]]).typeArgs
          e
        }

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        SerializationSuccess(JsonArray { appendElement =>
          val i = collection.iterator()
          while (i.hasNext) {
            val element = i.next()
            val SerializationSuccess(elementJson) = elementSerializer.serialize(new TaggedElement(element))
            appendElement(elementJson)
          }
        })
    }

}
