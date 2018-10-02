package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period
import java.time.format.DateTimeParseException

class PeriodDeserializer extends Deserializer[Period] {

  self =>

  private val PeriodType: Type = typeOf[Period]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Period] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Period.parse(x), PeriodType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e, reportedBy = self)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, PeriodType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = self))
    }

}
