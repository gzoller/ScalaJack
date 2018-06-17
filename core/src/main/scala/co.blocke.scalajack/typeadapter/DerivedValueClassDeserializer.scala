package co.blocke.scalajack
package typeadapter

class DerivedValueClassDeserializer[Wrapped, Unwrapped](
                                                         unwrappedDeserializer: Deserializer[Unwrapped],
                                                         wrap: Unwrapped => Wrapped
                                                        )(implicit wrappedTypeTag: TypeTag[Wrapped]) extends Deserializer[Wrapped] {

  private val wrappedType: Type = wrappedTypeTag.tpe

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Wrapped] =
    unwrappedDeserializer.deserialize(path, json) map {
      case TypeTagged(unwrapped) =>
        val wrapped = wrap(unwrapped)
        TypeTagged(wrapped, wrappedType)
    }

}
