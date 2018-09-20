package co.blocke.scalajack
package typeadapter

object ShortTypeAdapter extends TypeAdapter.=:=[Short] {
  override val deserializer: Deserializer[Short] = new ShortDeserializer
  override val serializer: Serializer[Short] = new ShortSerializer
}
