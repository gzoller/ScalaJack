package co.blocke.scalajack
package typeadapter

class EitherDeserializer[L, R](leftDeserializer: Deserializer[L], rightDeserializer: Deserializer[R])(implicit ttLeft: TypeTag[L], ttRight: TypeTag[R]) extends Deserializer[Either[L, R]] {

  private val LeftTypeConstructor: Type = typeOf[Left[_, _]].typeConstructor
  private val RightTypeConstructor: Type = typeOf[Right[_, _]].typeConstructor

  private val defaultLeftType: Type = ttLeft.tpe
  private val defaultRightRightType: Type = ttRight.tpe

  private class TaggedLeft(override val get: Left[L, R], taggedLeftValue: TypeTagged[L]) extends TypeTagged[Left[L, R]] {
    override lazy val tpe: Type = appliedType(LeftTypeConstructor, taggedLeftValue.tpe, defaultRightRightType)
  }

  private class TaggedRight(override val get: Right[L, R], taggedRightValue: TypeTagged[R]) extends TypeTagged[Right[L, R]] {
    override lazy val tpe: Type = appliedType(RightTypeConstructor, defaultLeftType, taggedRightValue.tpe)
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
