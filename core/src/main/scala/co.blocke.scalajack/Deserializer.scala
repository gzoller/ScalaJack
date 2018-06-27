package co.blocke.scalajack

object Deserializer {

  def constant[T](tagged: TypeTagged[T]): Deserializer[T] =
    new Deserializer[T] {

      override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
        DeserializationSuccess(tagged)

      override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
        DeserializationFailure(path, DeserializationError.Unsupported(s"Expected no JSON at path $path because value is constant"))

    }

}

trait Deserializer[+T] {

  def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    DeserializationFailure(path, DeserializationError.Unsupported("Cannot deserialize from nothing"))

  def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T]

}
