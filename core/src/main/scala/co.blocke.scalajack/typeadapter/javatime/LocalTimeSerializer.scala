package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.DateTimeFormatter

class LocalTimeSerializer(formatter: DateTimeFormatter) extends Serializer[LocalTime] {

  override def serialize[J](tagged: TypeTagged[LocalTime])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}
