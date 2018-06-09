package co.blocke.scalajack

trait Serializer[T] {

  def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J]

}
