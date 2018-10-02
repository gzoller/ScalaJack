package co.blocke.scalajack

class DeferredDeserializerReference[T](resolve: () => Deserializer[T]) extends Deserializer[T] {

  private lazy val resolved: Deserializer[T] = resolve()

  override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    resolved.deserializeFromNothing[J](path)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[T] =
    resolved.deserialize[J](path, json)

}
