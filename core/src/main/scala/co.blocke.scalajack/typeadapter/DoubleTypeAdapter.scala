package co.blocke.scalajack
package typeadapter

object DoubleTypeAdapter extends TypeAdapter.=:=[Double] {
  override val deserializer: Deserializer[Double] = new DoubleDeserializer
  override val serializer: Serializer[Double] = new DoubleSerializer
}
