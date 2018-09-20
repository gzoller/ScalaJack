package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object LocalDateTimeTypeAdapter extends LocalDateTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

class LocalDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDateTime] with StringKind {
  override val deserializer: Deserializer[LocalDateTime] = new LocalDateTimeDeserializer(formatter)
  override val serializer: Serializer[LocalDateTime] = new LocalDateTimeSerializer(formatter)
}
