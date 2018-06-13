package co.blocke.scalajack

class DeferredSerializerReference[T](resolve: () => Serializer[T]) extends Serializer[T] {

  private lazy val resolved: Serializer[T] = resolve()

  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
    resolved.serialize[J](tagged)

}
