package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalTime
import java.time.format.DateTimeFormatter

object LocalTimeTypeAdapter extends LocalTimeTypeAdapter(DateTimeFormatter.ISO_LOCAL_TIME)

class LocalTimeTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalTime] with StringKind {
  override val deserializer: Deserializer[LocalTime] = new LocalTimeDeserializer(formatter)
  override val serializer: Serializer[LocalTime] = new LocalTimeSerializer(formatter)
}
