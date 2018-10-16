package co.blocke.scalajack
package typeadapter

object OptionTypeAdapter extends TypeAdapterFactory.=:=.withOneTypeParam[Option] {

  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Option[E]], ttElement: TypeTag[E]): TypeAdapter[Option[E]] = {
    val valueTypeAdapter = context.typeAdapterOf[E]
    OptionTypeAdapter(
      new OptionDeserializer(valueTypeAdapter.deserializer),
      new OptionSerializer(valueTypeAdapter.serializer),
      valueTypeAdapter)
  }

}

// We need 3 types of Option adapters here:
//   1: The "normal" one writes nothing for None.  This eliminates the None from the structure, e.g. fields in objects.
//   2: "Empty" one writes "" for None.  This is used for Map keys that are None.
//   3: "Null" one converts None into null.  This is used mainly for Tuple members and Map values.
//

case class OptionTypeAdapter[T](override val deserializer: Deserializer[Option[T]], override val serializer: Serializer[Option[T]], valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]] {

  override def defaultValue: Option[Option[T]] = Some(None)

  // Must be called by parent of the Option when appropriate to get the null-writing version.
  def noneAsNull: TypeAdapter[Option[T]] = OptionTypeAdapterNull(valueTypeAdapter)
  def noneAsEmptyString: TypeAdapter[Option[T]] = OptionTypeAdapterEmpty(valueTypeAdapter)

}

case class OptionTypeAdapterNull[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]]

// This is for non-canonical map keys, which can be None --> rendered as ""
// Reads have to reverse-engineer the "" into a None
case class OptionTypeAdapterEmpty[T](valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Option[T]]
