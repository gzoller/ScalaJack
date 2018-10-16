package co.blocke.scalajack
package typeadapter

class OptionDeserializer[T](next: Deserializer[T]) extends Deserializer[Option[T]] {

  private val SomeTypeConstructor: Type = typeOf[Some[_]].typeConstructor
  private val TaggedNone: TypeTagged[None.type] = TypeTagged(None, typeOf[None.type])
  private val TaggedNull: TypeTagged[None.type] = TypeTagged(null.asInstanceOf[None.type], typeOf[None.type])

  private class TaggedSome(override val get: Some[T], taggedValue: TypeTagged[T]) extends TypeTagged[Some[T]] {
    override lazy val tpe: Type = appliedType(SomeTypeConstructor, taggedValue.tpe)
  }

  override def deserializeFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[Option[T]] =
    DeserializationSuccess(TaggedNone)

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Option[T]] =
    json match {
      case JsonNull() if (guidance.isMapKey) =>
        DeserializationSuccess(TaggedNull)
      case JsonNull() =>
        DeserializationSuccess(TaggedNone)
      case JsonString(s) if (s == "") =>
        DeserializationSuccess(TaggedNone)
      case _ =>
        next.deserialize(path, json) map {
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
