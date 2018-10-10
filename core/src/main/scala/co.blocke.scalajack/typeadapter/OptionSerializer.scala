package co.blocke.scalajack
package typeadapter

class OptionSerializer[T](next: Serializer[T]) extends Serializer[Option[T]] {

  private val OptionTypeSymbol: TypeSymbol = symbolOf[Option[_]]

  private class TaggedSomeValue(override val get: T, taggedOption: TypeTagged[Option[T]]) extends TypeTagged[T] {
    override lazy val tpe: Type = taggedOption.tpe.baseType(OptionTypeSymbol).typeArgs.head
  }

  override def serialize[J](tagged: TypeTagged[Option[T]])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(None) if (guidance.isMapKey) => SerializationSuccess(JsonString(""))
      case TypeTagged(None) if (guidance.inSeq || guidance.isMapValue) => SerializationSuccess(JsonNull())
      case TypeTagged(None) => SerializationFailure(SerializationError.Nothing)
      case TypeTagged(Some(value)) => next.serialize(new TaggedSomeValue(value, tagged))
    }
}
