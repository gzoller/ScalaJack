package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class LocalTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalTime] {

  self =>

  private val LocalTimeType: Type = typeOf[LocalTime]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalTime.parse(x, formatter), LocalTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
