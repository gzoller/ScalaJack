package co.blocke.scalajack
package typeadapter

case class FallbackTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    primaryTypeAdapter:        TypeAdapter[T],
    secondaryTypeAdapter:      TypeAdapter[T]) extends TypeAdapter[T]
