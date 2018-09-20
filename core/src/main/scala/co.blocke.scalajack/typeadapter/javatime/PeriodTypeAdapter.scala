package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Period

object PeriodTypeAdapter extends TypeAdapter.=:=[Period] with StringKind {
  override val deserializer: Deserializer[Period] = new PeriodDeserializer
  override val serializer: Serializer[Period] = new PeriodSerializer
}
