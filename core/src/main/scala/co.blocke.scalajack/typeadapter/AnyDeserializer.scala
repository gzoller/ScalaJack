package co.blocke.scalajack
package typeadapter

class AnyDeserializer(
    typeDeserializer:    Deserializer[Type],
    mapDeserializer:     Deserializer[Map[Any, Any]],
    listDeserializer:    Deserializer[List[Any]],
    stringDeserializer:  Deserializer[String],
    numberDeserializer:  Deserializer[java.lang.Number],
    booleanDeserializer: Deserializer[Boolean],
    context:             Context) extends Deserializer[Any] {

  private val nullTypeTagged = TypeTagged(null, typeOf[Any])

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: DeserializationGuidance): DeserializationResult[Any] =
    json match {
      case JsonObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        val concreteTypeFieldName = context.defaultHint

        ops.getObjectField(fields, concreteTypeFieldName) match {
          case Some(concreteTypeJson) =>
            val DeserializationSuccess(TypeTagged(concreteType)) = typeDeserializer.deserialize(path \ concreteTypeFieldName, concreteTypeJson)
            context.typeAdapter(concreteType).deserializer.deserialize(path, json)

          case None =>
            mapDeserializer.deserialize(path, json)
        }

      case JsonArray(_) =>
        listDeserializer.deserialize(path, json)

      case JsonString(_) =>
        stringDeserializer.deserialize(path, json)

      case JsonBoolean(_) =>
        booleanDeserializer.deserialize(path, json)

      case JsonDouble(_) | JsonDecimal(_) | JsonInt(_) | JsonLong(_) =>
        numberDeserializer.deserialize(path, json)

      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)
    }

}
