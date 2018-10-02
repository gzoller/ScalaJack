package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.DateTimeFormatter

class OffsetTimeSerializer(formatter: DateTimeFormatter) extends Serializer[OffsetTime] {

  override def serialize[J](tagged: TypeTagged[OffsetTime])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}
