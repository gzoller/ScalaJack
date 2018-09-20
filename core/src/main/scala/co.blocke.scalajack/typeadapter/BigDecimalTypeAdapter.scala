package co.blocke.scalajack
package typeadapter

object BigDecimalTypeAdapter extends TypeAdapter.=:=[BigDecimal] {
  override val deserializer: Deserializer[BigDecimal] = new BigDecimalDeserializer
  override val serializer: Serializer[BigDecimal] = new BigDecimalSerializer
}
