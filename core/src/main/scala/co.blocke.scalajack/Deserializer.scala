package co.blocke.scalajack

object Deserializer {

  def constant[T](tagged: TypeTagged[T]): Deserializer[T] =
    new Deserializer[T] {

      self =>

      override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
        DeserializationSuccess(tagged)

      override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[T] =
        DeserializationFailure(path, DeserializationError.Unsupported(s"Expected no JSON at path $path because value is constant", reportedBy = self))

    }

}

trait Deserializer[+T] {

  self =>

  def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    DeserializationFailure(path, DeserializationError.Missing(reportedBy = self))

  def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance = NormalGuidance): DeserializationResult[T]

}
