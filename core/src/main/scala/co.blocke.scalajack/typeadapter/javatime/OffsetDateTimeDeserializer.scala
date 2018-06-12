package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.OffsetDateTimeDeserializer.OffsetDateTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }

object OffsetDateTimeDeserializer {

  val OffsetDateTimeType: Type = typeOf[OffsetDateTime]

}

class OffsetDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[OffsetDateTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[OffsetDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(OffsetDateTime.parse(x, formatter), OffsetDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, OffsetDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
