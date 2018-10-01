package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class ZonedDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[ZonedDateTime] {

  self =>

  private val ZonedDateTimeType: Type = typeOf[ZonedDateTime]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[ZonedDateTime] =
    json match {
      case JsonNull() => DeserializationSuccess(TypeTagged(null, ZonedDateTimeType))
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(ZonedDateTime.parse(x, formatter), ZonedDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
