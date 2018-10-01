package co.blocke.scalajack
package typeadapter

import co.blocke.scalajack.BijectiveFunctions.fullNameToType

object AnyTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe =:= typeOf[Any]) {
      val typeTypeAdapter = context.typeAdapterOf[Type]
      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]
      val mapTypeAdapter = context.typeAdapterOf[Map[Any, Any]]
      val listTypeAdapter = context.typeAdapterOf[List[Any]]
      val stringTypeAdapter = context.typeAdapterOf[String]
      val numberTypeAdapter = context.typeAdapterOf[java.lang.Number]
      val booleanTypeAdapter = context.typeAdapterOf[Boolean]

      AnyTypeAdapter(
        new AnyDeserializer(
          typeTypeAdapter.deserializer,
          mapTypeAdapter.deserializer,
          listTypeAdapter.deserializer,
          stringTypeAdapter.deserializer,
          NumberDeserializer(),
          booleanTypeAdapter.deserializer,
          context),
        new AnySerializer(
          typeTypeAdapter.serializer,
          memberNameTypeAdapter.serializer,
          mapTypeAdapter.serializer,
          listTypeAdapter.serializer,
          stringTypeAdapter.serializer,
          numberTypeAdapter.serializer,
          booleanTypeAdapter.serializer,
          context),
        typeTypeAdapter,
        memberNameTypeAdapter,
        mapTypeAdapter,
        listTypeAdapter,
        stringTypeAdapter,
        booleanTypeAdapter,
        context).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class AnyTypeAdapter(
    override val deserializer: Deserializer[Any],
    override val serializer:   Serializer[Any],
    typeTypeAdapter:           TypeAdapter[Type],
    memberNameTypeAdapter:     TypeAdapter[MemberName],
    mapTypeAdapter:            TypeAdapter[Map[Any, Any]],
    listTypeAdapter:           TypeAdapter[List[Any]],
    stringTypeAdapter:         TypeAdapter[String],
    booleanTypeAdapter:        TypeAdapter[Boolean],
    context:                   Context) extends TypeAdapter.=:=[Any] {

  // For writes
  def inspectStringKind(value: Any): Boolean = value match {
    case string: String          => true
    case enum: Enumeration#Value => true
    case _                       => false
  }
}
