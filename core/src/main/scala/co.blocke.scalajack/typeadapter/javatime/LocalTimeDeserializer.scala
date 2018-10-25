package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class LocalTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalTime] {

  self =>

  private val LocalTimeType: Type = typeOf[LocalTime]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[LocalTime] =
    ast match {
      case AstString(x) =>
        DeserializationResult(path)(TypeTagged(LocalTime.parse(x, formatter), LocalTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, LocalTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
