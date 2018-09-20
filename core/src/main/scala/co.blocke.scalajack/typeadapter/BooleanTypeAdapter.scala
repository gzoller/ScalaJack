package co.blocke.scalajack
package typeadapter

object BooleanTypeAdapter extends TypeAdapter.=:=[Boolean] {
  override val deserializer: Deserializer[Boolean] = new BooleanDeserializer
  override val serializer: Serializer[Boolean] = new BooleanSerializer
}
