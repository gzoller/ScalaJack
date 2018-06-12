package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.LocalTimeDeserializer.LocalTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }

object LocalTimeDeserializer {

  val LocalTimeType: Type = typeOf[LocalTime]

}

class LocalTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalTime.parse(x, formatter), LocalTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalTimeType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
