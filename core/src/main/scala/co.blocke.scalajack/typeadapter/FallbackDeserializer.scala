package co.blocke.scalajack
package typeadapter

import scala.util.control.NonFatal

class FallbackDeserializer[T](
    primaryDeserializer:   Deserializer[T],
    secondaryDeserializer: Deserializer[T]) extends Deserializer[T] {

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[T] = {
    val primaryDeserializationResult = try primaryDeserializer.deserializeFromNothing(path) catch {
      case NonFatal(e) =>
        DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
    }

    primaryDeserializationResult match {
      case primarySuccess @ DeserializationSuccess(_) =>
        primarySuccess

      case DeserializationFailure(primaryErrors) =>
        val secondaryDeserializationResult = try secondaryDeserializer.deserializeFromNothing(path) catch {
          case NonFatal(e) =>
            DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
        }

        secondaryDeserializationResult match {
          case secondarySuccess @ DeserializationSuccess(_) =>
            secondarySuccess

          case DeserializationFailure(secondaryErrors) =>
            DeserializationFailure(primaryErrors ++ secondaryErrors)
        }
    }
  }

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] = {
    val primaryDeserializationResult = try primaryDeserializer.deserialize(path, ast) catch {
      case NonFatal(e) =>
        DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
    }

    primaryDeserializationResult match {
      case primarySuccess @ DeserializationSuccess(_) =>
        primarySuccess

      case DeserializationFailure(primaryErrors) =>
        val secondaryDeserializationResult = try secondaryDeserializer.deserialize(path, ast) catch {
          case NonFatal(e) =>
            DeserializationFailure(path, DeserializationError.ExceptionThrown(e))
        }

        secondaryDeserializationResult match {
          case secondarySuccess @ DeserializationSuccess(_) =>
            secondarySuccess

          case DeserializationFailure(secondaryErrors) =>
            DeserializationFailure(primaryErrors ++ secondaryErrors)
        }
    }
  }

}
