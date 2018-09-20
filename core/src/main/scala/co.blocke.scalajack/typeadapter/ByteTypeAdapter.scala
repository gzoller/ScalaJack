package co.blocke.scalajack
package typeadapter

object ByteTypeAdapter extends TypeAdapter.=:=[Byte] {
  override val deserializer: Deserializer[Byte] = new ByteDeserializer
  override val serializer: Serializer[Byte] = new ByteSerializer
}
