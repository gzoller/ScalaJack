package co.blocke.scalajack
package typeadapter

import scala.collection.GenTraversableOnce

class CollectionSerializer[E, C <: GenTraversableOnce[E]](elementSerializer: Serializer[E]) extends Serializer[C] {

  private val GenTraversableOnceTypeSymbol: TypeSymbol = symbolOf[GenTraversableOnce[_]]

  override def serialize[J](tagged: TypeTagged[C])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(collection) =>
        lazy val elementType: Type = tagged.tpe.baseType(GenTraversableOnceTypeSymbol).typeArgs.head

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        SerializationSuccess(JsonArray { appendElement =>
          for (element <- collection) {
            val SerializationSuccess(elementJson) = elementSerializer.serialize(new TaggedElement(element))
            appendElement(elementJson)
          }
        })
    }

}
