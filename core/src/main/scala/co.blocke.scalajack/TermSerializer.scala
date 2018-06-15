package co.blocke.scalajack

class TermSerializer[T](next: Serializer[T]) extends Serializer[T] {

  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] = ???

}
