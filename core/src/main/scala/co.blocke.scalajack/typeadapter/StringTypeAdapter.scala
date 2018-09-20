package co.blocke.scalajack
package typeadapter

object StringTypeAdapter extends TypeAdapter.=:=[String] with StringKind {
  override val deserializer: Deserializer[String] = new StringDeserializer
  override val serializer: Serializer[String] = new StringSerializer
}
