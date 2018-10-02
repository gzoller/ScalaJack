package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

class OffsetDateTimeSerializer(formatter: DateTimeFormatter) extends Serializer[OffsetDateTime] {

  override def serialize[J](tagged: TypeTagged[OffsetDateTime])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(JsonNull())
      case TypeTagged(x)    => SerializationSuccess(JsonString(x.format(formatter)))
    }

}
