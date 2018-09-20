package co.blocke.scalajack
package typeadapter

object BigIntTypeAdapter extends TypeAdapter.=:=[BigInt] {
  override val deserializer: Deserializer[BigInt] = new BigIntDeserializer
  override val serializer: Serializer[BigInt] = new BigIntSerializer
}
