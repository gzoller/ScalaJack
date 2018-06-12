package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period
import java.time.format.DateTimeParseException

object PeriodDeserializer {

  private val PeriodType: Type = typeOf[Period]

}

class PeriodDeserializer extends Deserializer[Period] {

  import PeriodDeserializer.PeriodType

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Period] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Period.parse(x), PeriodType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, PeriodType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
