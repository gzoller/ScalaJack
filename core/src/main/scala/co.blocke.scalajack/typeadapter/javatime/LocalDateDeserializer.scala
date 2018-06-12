package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.LocalDateDeserializer.LocalDateType

import scala.reflect.runtime.universe.{ Type, typeOf }

object LocalDateDeserializer {

  val LocalDateType: Type = typeOf[LocalDate]

}

class LocalDateDeserializer(formatter: DateTimeFormatter) extends Deserializer[LocalDate] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[LocalDate] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(LocalDate.parse(x, formatter), LocalDateType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })

      case JsonNull() =>
        DeserializationSuccess(TypeTagged(null, LocalDateType))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON string"))
    }

}
