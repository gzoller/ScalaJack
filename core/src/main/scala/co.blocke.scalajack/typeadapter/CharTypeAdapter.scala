package co.blocke.scalajack
package typeadapter

object CharTypeAdapter extends TypeAdapter.=:=[Char] with StringKind {
  override val deserializer: Deserializer[Char] = new CharDeserializer
  override val serializer: Serializer[Char] = new CharSerializer
}
