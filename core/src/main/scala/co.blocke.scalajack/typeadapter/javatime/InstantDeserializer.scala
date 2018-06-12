package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant
import java.time.format.DateTimeParseException

import co.blocke.scalajack.typeadapter.javatime.InstantDeserializer.InstantType

import scala.reflect.runtime.universe.{ Type, typeOf }

object InstantDeserializer {

  private val InstantType: Type = typeOf[Instant]

}

class InstantDeserializer extends Deserializer[Instant] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Instant] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(Instant.parse(x), InstantType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, InstantType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Malformed("Expected a JSON string"))
    }

}
