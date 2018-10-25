package co.blocke.scalajack
package typeadapter
package javacollections

class JavaCollectionSerializer[E, G <: java.util.Collection[E]](elementSerializer: Serializer[E]) extends Serializer[G] {

  override def serialize[AST, S](taggedCollection: TypeTagged[G])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    taggedCollection match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(collection) =>
        lazy val elementType: Type = {
          val e :: Nil = taggedCollection.tpe.baseType(symbolOf[java.util.Collection[_]]).typeArgs
          e
        }

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        SerializationSuccess(AstArray { appendElement =>
          val i = collection.iterator()
          while (i.hasNext) {
            val element = i.next()
            val SerializationSuccess(elementAst) = elementSerializer.serialize(new TaggedElement(element))
            appendElement(elementAst)
          }
        })
    }

}
