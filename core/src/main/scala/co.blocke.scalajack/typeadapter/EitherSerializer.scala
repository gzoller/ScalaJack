package co.blocke.scalajack
package typeadapter

class EitherSerializer[L, R](leftSerializer: Serializer[L], rightSerializer: Serializer[R]) extends Serializer[Either[L, R]] {

  private val EitherSymbol: Symbol = symbolOf[Either[_, _]]

  private class TaggedLeftValue(override val get: L, taggedEither: TypeTagged[Either[L, R]]) extends TypeTagged[L] {
    override lazy val tpe: Type = {
      val leftType :: _ :: Nil = taggedEither.tpe.baseType(EitherSymbol).typeArgs
      leftType
    }
  }

  private class TaggedRightValue(override val get: R, taggedEither: TypeTagged[Either[L, R]]) extends TypeTagged[R] {
    override lazy val tpe: Type = {
      val _ :: rightType :: Nil = taggedEither.tpe.baseType(EitherSymbol).typeArgs
      rightType
    }
  }

  override def serialize[AST, S](taggedEither: TypeTagged[Either[L, R]])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    taggedEither match {
      case TypeTagged(Left(leftValue)) =>
        leftSerializer.serialize(new TaggedLeftValue(leftValue, taggedEither))

      case TypeTagged(Right(rightValue)) =>
        rightSerializer.serialize(new TaggedRightValue(rightValue, taggedEither))

      case TypeTagged(null) => SerializationSuccess(AstNull())

    }

}
