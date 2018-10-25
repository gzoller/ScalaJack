package co.blocke.scalajack
package typeadapter

import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class TryDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[Try[T]] {

  private val successTypeConstructor: Type = typeOf[Success[_]].typeConstructor
  private val failureType: Type = appliedType(typeOf[Failure[_]].typeConstructor, tt.tpe)

  private class TaggedSuccess(override val get: Success[T], taggedValue: TypeTagged[T]) extends TypeTagged[Success[T]] {
    override lazy val tpe: Type = appliedType(successTypeConstructor, taggedValue.tpe)
  }

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Try[T]] =
    try {
      val deserializationResult = next.deserialize(path, ast) map {
        case tagged @ TypeTagged(value) =>
          new TaggedSuccess(Success(value), tagged)
      }

      deserializationResult match {
        case DeserializationSuccess(_) =>
          deserializationResult

        case deserializationFailure @ DeserializationFailure(_) =>
          val exceptionWithBackingAstValue = new DeserializationException(deserializationFailure) with BackedByAstValue {
            override type BackingAstValue = AST
            override type SrcType = S
            override val backingAstValue: BackingAstValue = ast
            override val backingAstOps: AstOps[BackingAstValue, SrcType] = ops
          }
          DeserializationSuccess(TypeTagged(Failure(exceptionWithBackingAstValue), failureType))
      }
    } catch {
      case exception: DeserializationException with BackedByAstValue =>
        DeserializationSuccess(TypeTagged(Failure(exception), failureType))

      case exception: DeserializationException =>
        val exceptionWithBackingAstValue = new DeserializationException(exception.deserializationFailure) with BackedByAstValue {
          override type BackingAstValue = AST
          override type SrcType = S
          override val backingAstValue: BackingAstValue = ast
          override val backingAstOps: AstOps[BackingAstValue, SrcType] = ops
        }
        DeserializationSuccess(TypeTagged(Failure(exceptionWithBackingAstValue), failureType))

      case NonFatal(e) =>
        DeserializationSuccess(TypeTagged(Failure(e), failureType))
    }

}
