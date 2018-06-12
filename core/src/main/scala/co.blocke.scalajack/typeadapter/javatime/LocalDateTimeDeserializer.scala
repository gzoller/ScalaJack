package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class LocalDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDateTime] {

  private val LocalDateTimeType: Type = typeOf[LocalDateTime]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDateTime.parse(x, formatter), LocalDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
