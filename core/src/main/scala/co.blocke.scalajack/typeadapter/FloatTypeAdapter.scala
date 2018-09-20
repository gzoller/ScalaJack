package co.blocke.scalajack
package typeadapter

object FloatTypeAdapter extends TypeAdapter.=:=[Float] {
  override val deserializer: Deserializer[Float] = new FloatDeserializer
  override val serializer: Serializer[Float] = new FloatSerializer
}
