package co.blocke.scalajack
package typeadapter

class OptionDeserializer[T](next: Deserializer[T]) extends Deserializer[Option[T]] {

  private val SomeTypeConstructor: Type = typeOf[Some[_]].typeConstructor
  private val TaggedNone: TypeTagged[None.type] = TypeTagged(None, typeOf[None.type])
  private val TaggedNull: TypeTagged[None.type] = TypeTagged(null.asInstanceOf[None.type], typeOf[None.type])

  private class TaggedSome(override val get: Some[T], taggedValue: TypeTagged[T]) extends TypeTagged[Some[T]] {
    override lazy val tpe: Type = appliedType(SomeTypeConstructor, taggedValue.tpe)
  }

  override def deserializeFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[Option[T]] =
    DeserializationSuccess(TaggedNone)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Option[T]] =
    ast match {
      case AstNull() if (guidance.isMapKey) =>
        DeserializationSuccess(TaggedNull)
      case AstNull() =>
        DeserializationSuccess(TaggedNone)
      case AstString(s) if (s == "") =>
        DeserializationSuccess(TaggedNone)
      case _ =>
        next.deserialize(path, ast) map {
          case tagged @ TypeTagged(value) =>
            Option(value) match {
              case None =>
                TaggedNone
              case some @ Some(_) =>
                new TaggedSome(some, tagged)
            }
        }
    }

}
