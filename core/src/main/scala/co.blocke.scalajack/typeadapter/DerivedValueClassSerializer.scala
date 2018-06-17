package co.blocke.scalajack
package typeadapter

class DerivedValueClassSerializer[Wrapped, Unwrapped](
                                                       unwrap: Wrapped => Unwrapped,
                                                       unwrappedSerializer: Serializer[Unwrapped]
                                                           )(implicit unwrappedTypeTag: TypeTag[Unwrapped]) extends Serializer[Wrapped] {

  private val unwrappedType: Type = unwrappedTypeTag.tpe

  override def serialize[J](tagged: TypeTagged[Wrapped])(implicit ops: JsonOps[J]): SerializationResult[J] =
    tagged match {
      case TypeTagged(wrapped) =>
        val unwrapped = unwrap(wrapped)
        unwrappedSerializer.serialize(TypeTagged(unwrapped, unwrappedType))
    }

}
