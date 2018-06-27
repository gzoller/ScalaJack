package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration
import java.time.format.DateTimeParseException

class DurationDeserializer extends Deserializer[Duration] {

  self =>

  private val DurationType: Type = typeOf[Duration]

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Duration] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Duration.parse(x), DurationType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, DurationType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string", reportedBy = Some(self)))
    }

}
