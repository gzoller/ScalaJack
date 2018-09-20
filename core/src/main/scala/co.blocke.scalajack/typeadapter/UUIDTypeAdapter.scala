package co.blocke.scalajack
package typeadapter

import java.util.UUID

object UUIDTypeAdapter extends TypeAdapter.=:=[UUID] {
  override val deserializer: Deserializer[UUID] = new UUIDDeserializer
  override val serializer: Serializer[UUID] = new UUIDSerializer
}
