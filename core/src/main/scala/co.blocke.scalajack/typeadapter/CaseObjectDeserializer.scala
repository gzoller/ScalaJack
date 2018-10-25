package co.blocke.scalajack
package typeadapter

class CaseObjectDeserializer[C](subclasses: List[String])(implicit tt: TypeTag[C]) extends Deserializer[C] {

  self =>

  private val CaseObjectType: Type = typeOf[C]
  private val taggedNull: TypeTagged[C] = TypeTagged(null.asInstanceOf[C], tt.tpe)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[C] =
    ast match {
      case AstNull() => DeserializationSuccess(taggedNull)

      case AstString(s) if (subclasses.contains(s)) =>
        val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
        val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[C]
        DeserializationSuccess(TypeTagged(objInstance, typeOf[C]))

      case _ => DeserializationFailure(path, DeserializationError.Unexpected(s"Expected a valid subclass of $CaseObjectType", reportedBy = self))
    }
}