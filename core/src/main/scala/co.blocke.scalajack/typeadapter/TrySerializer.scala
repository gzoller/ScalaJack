package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

class TrySerializer[T](next: Serializer[T]) extends Serializer[Try[T]] {

  private val TryTypeSymbol: TypeSymbol = symbolOf[Try[_]]

  private class TaggedSuccessValue(override val get: T, taggedTry: TypeTagged[Try[T]]) extends TypeTagged[T] {
    override lazy val tpe: Type = taggedTry.tpe.baseType(TryTypeSymbol).typeArgs.head
  }

  override def serialize[AST, S](tagged: TypeTagged[Try[T]])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(Success(value)) =>
        next.serialize(new TaggedSuccessValue(value, tagged))

      case TypeTagged(Failure(e: BackedByAstValue)) =>
        SerializationSuccess(e.backingAstValueAs[AST, S])

      case TypeTagged(Failure(e)) =>
        throw e
    }

}
