package co.blocke.scalajack
package typeadapter

class EitherDeserializer[L, R](leftDeserializer: Deserializer[L], rightDeserializer: Deserializer[R])(implicit ttLeft: TypeTag[L], ttRight: TypeTag[R]) extends Deserializer[Either[L, R]] {

  private val leftTypeConstructor: Type = typeOf[Left[_, _]].typeConstructor
  private val rightTypeConstructor: Type = typeOf[Right[_, _]].typeConstructor

  private val defaultLeftValueType: Type = ttLeft.tpe
  private val defaultRightValueType: Type = ttRight.tpe

  private class TaggedLeft(override val get: Left[L, R], taggedLeftValue: TypeTagged[L]) extends TypeTagged[Left[L, R]] {
    override lazy val tpe: Type = appliedType(leftTypeConstructor, taggedLeftValue.tpe, defaultRightValueType)
  }

  private class TaggedRight(override val get: Right[L, R], taggedRightValue: TypeTagged[R]) extends TypeTagged[Right[L, R]] {
    override lazy val tpe: Type = appliedType(rightTypeConstructor, defaultLeftValueType, taggedRightValue.tpe)
  }

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Either[L, R]] =
    rightDeserializer.deserialize(path, json) match {
      case DeserializationSuccess(taggedRightValue @ TypeTagged(rightValue)) =>
        DeserializationSuccess(new TaggedRight(Right(rightValue), taggedRightValue))

      case DeserializationFailure(rightErrors) =>
        leftDeserializer.deserialize(path, json) match {
          case DeserializationSuccess(taggedLeftValue @ TypeTagged(leftValue)) =>
            DeserializationSuccess(new TaggedLeft(Left(leftValue), taggedLeftValue))

          case DeserializationFailure(leftErrors) =>
            DeserializationFailure(rightErrors ++ leftErrors)
        }
    }

}
