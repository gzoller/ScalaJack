package co.blocke.scalajack
package typeadapter

class CaseObjectDeserializer[C](subclasses: List[String])(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private val CaseObjectType: Type = typeOf[C]
  private val taggedNull: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], tt.tpe)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[C] =
    json match {
      case JsonNull() => DeserializationSuccess(taggedNull)

      case JsonString(s) if (subclasses.contains(s)) =>
        val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
        val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[C]
        DeserializationSuccess(TypeTagged(objInstance, typeOf[C]))

      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a valid subclass of $CaseObjectType", reportedBy = self))
    }
}