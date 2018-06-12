package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration
import java.time.format.DateTimeParseException

import co.blocke.scalajack.typeadapter.javatime.DurationDeserializer.DurationType

import scala.reflect.runtime.universe.{ Type, typeOf }

object DurationDeserializer {

  val DurationType: Type = typeOf[Duration]

}

class DurationDeserializer extends Deserializer[Duration] {

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
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
