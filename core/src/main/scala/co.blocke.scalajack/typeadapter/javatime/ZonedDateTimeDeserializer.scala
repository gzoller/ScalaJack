package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

import co.blocke.scalajack.typeadapter.javatime.ZonedDateTimeDeserializer.ZonedDateTimeType

import scala.reflect.runtime.universe.{ Type, typeOf }

object ZonedDateTimeDeserializer {

  val ZonedDateTimeType: Type = typeOf[ZonedDateTime]

}

class ZonedDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[ZonedDateTime] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[ZonedDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(ZonedDateTime.parse(x, formatter), ZonedDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })
    }

}
