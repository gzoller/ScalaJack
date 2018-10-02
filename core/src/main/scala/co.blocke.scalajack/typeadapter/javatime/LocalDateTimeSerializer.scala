package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class LocalDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[LocalDateTime] {

  override def serialize[J](tagged: TypeTagged[LocalDateTime])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}
