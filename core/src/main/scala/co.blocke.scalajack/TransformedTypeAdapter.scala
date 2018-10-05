package co.blocke.scalajack

case class TransformedTypeAdapter[A, B](
    typeAdapter: TypeAdapter[A],
    f:           BijectiveFunction[A, B])(implicit context: Context, ttB: TypeTag[B]) extends TypeAdapter[B] {

  // PROBLEM: We're ot using f!  This would be the type modifier.  We need a wrapping ser/deser that exercises f then
  // feeds into myTypeAdapter.ser/deser.

  private val myTypeAdapter = context.typeAdapterOf[B]
  override val serializer: Serializer[B] = myTypeAdapter.serializer
  override val deserializer: Deserializer[B] = myTypeAdapter.deserializer
}
