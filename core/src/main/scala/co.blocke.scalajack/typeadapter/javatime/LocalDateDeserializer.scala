package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class LocalDateDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDate] {

  self =>

  private val LocalDateType: Type = typeOf[LocalDate]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[LocalDate] =
    ast match {
      case AstString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDate.parse(x, formatter), LocalDateType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
