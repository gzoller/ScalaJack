package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

class OffsetTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[OffsetTime] {

  self =>

  private val OffsetTimeType: Type = typeOf[OffsetTime]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[OffsetTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(OffsetTime.parse(x, formatter), OffsetTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, OffsetTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = Some(self)))
    }

}
