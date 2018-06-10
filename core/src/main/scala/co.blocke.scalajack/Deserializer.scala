package co.blocke.scalajack

trait Deserializer[+T] {

  def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    DeserializationFailure(path, DeserializationError.Missing)

  def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T]

}
