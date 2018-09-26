package co.blocke.scalajack
package typeadapter

import scala.util.{ Failure, Success, Try }

/**
 * A deserializer that will try parsing a JSON string into JSON AST if the next deserializer doesn't support the original JSON AST.
 *
 * @param next
 * @tparam T
 */
class JsonParsingFallbackDeserializer[T](next: Deserializer[T])(implicit tt: TypeTag[T]) extends Deserializer[T] {

  private val isNullSupported: Boolean = typeOf[Null] <:< tt.tpe
  private val taggedNull = TypeTagged(null.asInstanceOf[T], tt.tpe)

  override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    next.deserializeFromNothing(path) // TODO any fall-backs here?

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[T] = {
    next.deserialize(path, json) match {
      case deserializationSuccess @ DeserializationSuccess(_) =>
        deserializationSuccess

      case deserializationFailure @ DeserializationFailure(errors) if deserializationFailure.isUnexpected(path) =>
        json match {
          case JsonString(string) if (guidance.isMapKey || guidance.secondLookParsing) =>
            Try(JsonParser.parse(string)) match {
              case Success(Some(fallbackJson)) =>
                next.deserialize(path, fallbackJson) match {
                  case deserializationSuccess @ DeserializationSuccess(_) =>
                    deserializationSuccess

                  case DeserializationFailure(fallbackErrors) =>
                    DeserializationFailure(errors ++ fallbackErrors)
                }

              case Success(None) =>
                next.deserializeFromNothing(path) match {
                  case deserializationSuccess @ DeserializationSuccess(_) =>
                    deserializationSuccess

                  case fallbackDeserializationError @ DeserializationFailure(_) if isNullSupported && fallbackDeserializationError.isUnsupported(path) =>
                    DeserializationSuccess(taggedNull)

                  case DeserializationFailure(fallbackErrors) =>
                    DeserializationFailure(errors ++ fallbackErrors)
                }

              case Failure(e) =>
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
