package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration
import java.time.format.DateTimeParseException

class DurationDeserializer extends Deserializer[Duration] {

  self =>

  private val DurationType: Type = typeOf[Duration]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Duration] =
    ast match {
      case AstString(x) =>
        DeserializationResult(path)(TypeTagged(Duration.parse(x), DurationType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, DurationType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
