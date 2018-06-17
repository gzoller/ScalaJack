package co.blocke.scalajack
package typeadapter

class DerivedValueClassDeserializer[Derived, Source](
    sourceDeserializer: Deserializer[Source],
    derive:             Source => Derived)(implicit derivedTypeTag: TypeTag[Derived]) extends Deserializer[Derived] {

  private val derivedType: Type = derivedTypeTag.tpe

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Derived] =
    sourceDeserializer.deserialize(path, json) map {
      case TypeTagged(source) =>
        val derived = derive(source)
        TypeTagged(derived, derivedType)
    }

}
