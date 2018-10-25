package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

/**
 * A deserializer that will try parsing a JSON string into JSON AST if the next deserializer doesn't support the original JSON AST.
 *
 * @param next
 * @tparam T
 */
class AstParsingFallbackDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[T] {

  private val isNullSupported: Boolean = typeOf[Null] <:< tt.tpe
  private val taggedNull = TypeTagged(null.asInstanceOf[T], tt.tpe)

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] =
    next.deserializeFromNothing(path) // TODO any fall-backs here?

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] = {
    next.deserialize(path, ast) match {
      case deserializationSuccess @ DeserializationSuccess(_) =>
        deserializationSuccess

      case deserializationFailure @ DeserializationFailure(errors) if deserializationFailure.isUnexpected(path) =>
        ast match {
          case AstString(string) if (guidance.isMapKey || guidance.secondLookParsing) =>
            Try(ops.parse(string.asInstanceOf[S])) match {
              case Success(fallbackAst) =>
                next.deserialize(path, fallbackAst) match {
                  case deserializationSuccess @ DeserializationSuccess(_) =>
                    deserializationSuccess

                  case DeserializationFailure(_) =>
                    DeserializationFailure(errors)
                  // Note: We don't accumulate errors here because the initial error is considered ok
                  // for map keys or second look parsing.
                }

              case Failure(_) =>
                deserializationFailure
            }

          case _ =>
            deserializationFailure
        }

      case deserializationFailure @ DeserializationFailure(_) =>
        deserializationFailure
    }
  }

}
