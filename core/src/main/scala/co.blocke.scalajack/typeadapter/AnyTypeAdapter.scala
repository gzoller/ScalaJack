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
          memberNameTypeAdapter.deserializer,
          mapTypeAdapter.deserializer,
          listTypeAdapter.deserializer,
          stringTypeAdapter.deserializer,
          numberTypeAdapter.deserializer,
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

  override def read(reader: Reader): Any = {
    reader.peek match {
      case TokenType.BeginObject =>
        val savedPos = reader.position
        val mapRead = mapTypeAdapter.read(reader)
        // See if it's a serialized class (with default type hint).  Create class if so.
        val optionalClassType = mapRead.get(context.defaultHint).map(hint => fullNameToType.apply(hint.asInstanceOf[String]))
        optionalClassType match {
          case Some(t) =>
            reader.position = savedPos
            val classConstructed = context.typeAdapter(t).read(reader)
            classConstructed
          case None => mapRead
        }

      case TokenType.BeginArray =>
        listTypeAdapter.read(reader)

      case TokenType.String =>
        stringTypeAdapter.read(reader)

      case TokenType.True | TokenType.False =>
        booleanTypeAdapter.read(reader)

      case TokenType.Number =>
        reader.readNumber() // Use Scala numerical inference (see Reader.readNumber())

      case TokenType.Null =>
        reader.readNull()

    }
  }

  override def write(value: Any, writer: Writer): Unit = {
    // TODO come up with a better way to obtain the value's type
    value match {
      case null =>
        writer.writeNull()

      case string: String =>
        stringTypeAdapter.write(string, writer)

      case enum: Enumeration#Value =>
        stringTypeAdapter.write(enum.toString, writer)

      case list: List[_] =>
        listTypeAdapter.write(list, writer)

      case map: Map[_, _] =>
        mapTypeAdapter.write(map.asInstanceOf[Map[Any, Any]], writer)

      case _ =>
        val valueType = staticClass(value.getClass.getName).toType
        //    val valueType = currentMirror.reflectClass(currentMirror.classSymbol(value.getClass)).symbol.info
        //    val valueType = currentMirror.reflect(value)(ClassTag(value.getClass)).symbol.info

        val valueTypeAdapter = context.typeAdapter(valueType)

        val polymorphicWriter = new PolymorphicWriter(writer, "_hint", valueType, typeTypeAdapter, memberNameTypeAdapter)

        valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(value, polymorphicWriter)
    }
  }

}
