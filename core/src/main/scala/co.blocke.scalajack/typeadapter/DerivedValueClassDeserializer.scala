package co.blocke.scalajack
package typeadapter

class DerivedValueClassDeserializer[Derived, Source](
    sourceDeserializer: Deserializer[Source],
    derive:             Source => Derived)(implicit derivedTypeTag: TypeTag[Derived], sourceTypeTag: TypeTag[Source]) extends Deserializer[Derived] {

  private val derivedType: Type = derivedTypeTag.tpe

  override def toString: String = s"DerivedValueClassDeserializer[${derivedTypeTag.tpe}, ${sourceTypeTag.tpe}]"

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Derived] =
    sourceDeserializer.deserialize(path, ast) map {
      case TypeTagged(source) =>
        val derived = derive(source)
        TypeTagged(derived, derivedType)
    }
}
