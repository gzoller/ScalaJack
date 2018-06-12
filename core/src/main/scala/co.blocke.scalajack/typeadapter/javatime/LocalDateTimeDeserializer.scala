package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.LocalDateTimeDeserializer.LocalDateTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }

object LocalDateTimeDeserializer {

  val LocalDateTimeType: Type = typeOf[LocalDateTime]

}

class LocalDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDateTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDateTime.parse(x, formatter), LocalDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
