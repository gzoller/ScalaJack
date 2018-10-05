package co.blocke.scalajack

class TransformedSerializer[T](wrappedSerializer: Serializer[T]) extends Serializer[T] {

  def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] = {
    wrappedSerializer.serialize(tagged)
  }

}
