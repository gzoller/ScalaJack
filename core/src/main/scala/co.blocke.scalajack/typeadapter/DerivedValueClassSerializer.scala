package co.blocke.scalajack
package typeadapter

class DerivedValueClassSerializer[Derived, Source](
    unwrap:           Derived => Source,
    sourceSerializer: Serializer[Source])(implicit sourceTypeTag: TypeTag[Source]) extends Serializer[Derived] {

  private val sourceType: Type = sourceTypeTag.tpe

  override def serialize[AST, S](tagged: TypeTagged[Derived])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(derived) =>
        val source = unwrap(derived)
        sourceSerializer.serialize(TypeTagged(source, sourceType))
    }

}
