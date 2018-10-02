package co.blocke.scalajack
package typeadapter

class DerivedValueClassDeserializer[Derived, Source](
    sourceDeserializer: Deserializer[Source],
    derive:             Source => Derived)(implicit derivedTypeTag: TypeTag[Derived], sourceTypeTag: TypeTag[Source]) extends Deserializer[Derived] {

  private val derivedType: Type = derivedTypeTag.tpe

  override def toString: String = s"DerivedValueClassDeserializer[${derivedTypeTag.tpe}, ${sourceTypeTag.tpe}]"

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Derived] =
    sourceDeserializer.deserialize(path, json) map {
      case TypeTagged(source) =>
        val derived = derive(source)
        TypeTagged(derived, derivedType)
    }
}
