package co.blocke.scalajack
package typeadapter
package javatime

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object LocalDateTypeAdapter extends LocalDateTypeAdapter(DateTimeFormatter.ISO_LOCAL_DATE)

class LocalDateTypeAdapter(formatter: DateTimeFormatter) extends TypeAdapter.=:=[LocalDate] with StringKind {
  override val deserializer: Deserializer[LocalDate] = new LocalDateDeserializer(formatter)
  override val serializer: Serializer[LocalDate] = new LocalDateSerializer(formatter)
}
