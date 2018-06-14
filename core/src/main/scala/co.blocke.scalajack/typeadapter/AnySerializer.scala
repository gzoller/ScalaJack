package co.blocke.scalajack
package typeadapter

class AnySerializer(
    typeSerializer:       Serializer[Type],
    memberNameSerializer: Serializer[MemberName],
    mapSerializer:        Serializer[Map[Any, Any]],
    listSerializer:       Serializer[List[Any]],
    stringSerializer:     Serializer[String],
    numberSerializer:     Serializer[java.lang.Number],
    booleanSerializer:    Serializer[Boolean],
    context:              Context) extends Serializer[Any] {

  private val StringType: Type = typeOf[String]
  private val TypeType: Type = typeOf[Type]

  override def serialize[J](tagged: TypeTagged[Any])(implicit ops: JsonOps[J]): SerializationResult[J] =
    // TODO come up with a better way to obtain the value's type
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(_: String) =>
        stringSerializer.serialize(tagged.asInstanceOf[TypeTagged[String]])

      case TypeTagged(enum: Enumeration#Value) =>
        stringSerializer.serialize(TypeTagged(enum.toString, StringType))

      case TypeTagged(_: List[_]) =>
        listSerializer.serialize(tagged.asInstanceOf[TypeTagged[List[Any]]])

      case TypeTagged(_: Map[_, _]) =>
        mapSerializer.serialize(tagged.asInstanceOf[TypeTagged[Map[Any, Any]]])

      case TypeTagged(value) =>
        val valueType = staticClass(value.getClass.getName).toType
        //    val valueType = currentMirror.reflectClass(currentMirror.classSymbol(value.getClass)).symbol.info
        //    val valueType = currentMirror.reflect(value)(ClassTag(value.getClass)).symbol.info

        val valueSerializer = context.serializer(valueType).asInstanceOf[Serializer[Any]]

        valueSerializer.serialize(tagged) map {
          case JsonObject(x) =>
            val fields = x.asInstanceOf[ops.ObjectFields]

            val SerializationSuccess(typeJson) = typeSerializer.serialize(TypeTagged(valueType, TypeType))

            JsonObject { appendField =>
              appendField("_hint", typeJson)
              ops.foreachObjectField(fields, { (name, value) =>
                appendField(name, value)
              })
            }

          case json => json
        }
    }

}
