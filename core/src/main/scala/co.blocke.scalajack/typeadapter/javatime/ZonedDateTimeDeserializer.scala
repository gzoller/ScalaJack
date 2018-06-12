package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.{ DateTimeFormatter, DateTimeParseException }

object ZonedDateTimeDeserializer {

  private val ZonedDateTimeType: Type = typeOf[ZonedDateTime]

}

class ZonedDateTimeDeserializer(formatter: DateTimeFormatter) extends Deserializer[ZonedDateTime] {

  import ZonedDateTimeDeserializer.ZonedDateTimeType

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[ZonedDateTime] =
    json match {
      case JsonString(x) =>
        DeserializationResult(path)(TypeTagged(ZonedDateTime.parse(x, formatter), ZonedDateTimeType), {
          case e: DateTimeParseException =>
            DeserializationError.Malformed(e)
        })
    }

}
