package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class OffsetDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[OffsetDateTime] {

  self =>

  private val OffsetDateTimeType: Type = typeOf[OffsetDateTime]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[OffsetDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(OffsetDateTime.parse(x, formatter), OffsetDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, OffsetDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
