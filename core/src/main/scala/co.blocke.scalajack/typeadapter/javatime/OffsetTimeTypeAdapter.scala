package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetTime
import java.time.format.DateTimeFormatter

object OffsetTimeTypeAdapter extends OffsetTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_TIME)

class OffsetTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetTime] with StringKind {
  override val deserializer: Deserializer[OffsetTime] = new OffsetTimeDeserializer(formatter)
  override val serializer: Serializer[OffsetTime] = new OffsetTimeSerializer(formatter)
}
