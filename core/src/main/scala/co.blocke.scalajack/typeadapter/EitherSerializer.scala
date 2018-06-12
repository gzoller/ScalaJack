package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.typeadapter.EitherSerializer.EitherSymbol

import scala.reflect.runtime.universe.{ Symbol, Type, symbolOf }

object EitherSerializer {

  val EitherSymbol: Symbol = symbolOf[Either[_, _]]

}

class EitherSerializer[L, R](leftSerializer: Serializer[L], rightSerializer: Serializer[R]) extends Serializer[Either[L, R]] {

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

  override def serialize[J](taggedEither: TypeTagged[Either[L, R]])(implicit ops: JsonOps[J]): SerializationResult[J] =
    taggedEither match {
      case TypeTagged(Left(leftValue)) =>
        leftSerializer.serialize(new TaggedLeftValue(leftValue, taggedEither))

      case TypeTagged(Right(rightValue)) =>
        rightSerializer.serialize(new TaggedRightValue(rightValue, taggedEither))
    }

}
