package co.blocke.scalajack
package typeadapter
package javatime

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter

object OffsetDateTimeTypeAdapter extends OffsetDateTimeTypeAdapter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

class OffsetDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[OffsetDateTime] with StringKind {
  override val deserializer: Deserializer[OffsetDateTime] = new OffsetDateTimeDeserializer(formatter)
  override val serializer: Serializer[OffsetDateTime] = new OffsetDateTimeSerializer(formatter)
}
