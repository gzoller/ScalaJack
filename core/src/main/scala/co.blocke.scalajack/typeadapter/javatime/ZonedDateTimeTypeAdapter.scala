package co.blocke.scalajack
package typeadapter
package javatime

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object ZonedDateTimeTypeAdapter extends ZonedDateTimeTypeAdapter(DateTimeFormatter.ISO_ZONED_DATE_TIME)

class ZonedDateTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[ZonedDateTime] with StringKind {
  override val deserializer: Deserializer[ZonedDateTime] = new ZonedDateTimeDeserializer(formatter)
  override val serializer: Serializer[ZonedDateTime] = new ZonedDateTimeSerializer(formatter)
}
