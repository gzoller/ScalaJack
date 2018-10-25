package co.blocke.scalajack
package typeadapter

import scala.collection.GenTraversableOnce

class CollectionSerializer[E, C <: GenTraversableOnce[E]](elementSerializer: Serializer[E]) extends Serializer[C] {

  private val GenTraversableOnceTypeSymbol: TypeSymbol = symbolOf[GenTraversableOnce[_]]

  override def serialize[AST, S](tagged: TypeTagged[C])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(collection) =>
        lazy val elementType: Type = tagged.tpe.baseType(GenTraversableOnceTypeSymbol).typeArgs.head

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        SerializationSuccess(AstArray { appendElement =>
          for (element <- collection) {
            val SerializationSuccess(elementAst) = elementSerializer.serialize(new TaggedElement(element))(ops, guidance.withSeq())
            appendElement(elementAst)
          }
        })
    }

}
