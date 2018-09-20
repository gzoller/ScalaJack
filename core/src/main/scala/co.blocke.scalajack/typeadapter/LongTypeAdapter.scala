package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends TypeAdapter.=:=[Long] {
  override val deserializer: Deserializer[Long] = new LongDeserializer
  override val serializer: Serializer[Long] = new LongSerializer
}
