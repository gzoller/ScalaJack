package co.blocke.scalajack
package typeadapter

class OptionSerializer[T](next: Serializer[T]) extends Serializer[Option[T]] {

  private val OptionTypeSymbol: TypeSymbol = symbolOf[Option[_]]

  private class TaggedSomeValue(override val get: T, taggedOption: TypeTagged[Option[T]]) extends TypeTagged[T] {
    override lazy val tpe: Type = taggedOption.tpe.baseType(OptionTypeSymbol).typeArgs.head
  }

  override def serialize[AST, S](tagged: TypeTagged[Option[T]])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(None) if (guidance.isMapKey) => SerializationSuccess(AstString(""))
      case TypeTagged(None) if (guidance.inSeq || guidance.isMapValue) => SerializationSuccess(AstNull())
      case TypeTagged(None) => SerializationFailure(SerializationError.Nothing)
      case TypeTagged(Some(value)) => next.serialize(new TaggedSomeValue(value, tagged))
    }
}
