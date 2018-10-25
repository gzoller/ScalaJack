package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class OffsetDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[OffsetDateTime] {

  self =>

  private val OffsetDateTimeType: Type = typeOf[OffsetDateTime]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[OffsetDateTime] =
    ast match {
      case AstString(x) =>
        DeserializationResult(path)(TypeTagged(OffsetDateTime.parse(x, formatter), OffsetDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, OffsetDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
