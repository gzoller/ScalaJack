package co.blocke.scalajack
package typeadapter

import scala.collection.mutable

class TraitDeserializer[T](
    typeFieldName:    MemberName,
    typeDeserializer: Deserializer[Type], // Ignored for modified versions, as it is set by concrete type
    f:                Option[BijectiveFunction[String, Type]] = None
)(implicit tt: TypeTag[T], context: Context) extends Deserializer[T] {

  self =>

  private val polymorphicType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[T] = TypeTagged[T](null.asInstanceOf[T], polymorphicType)

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[T] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        var maybeConcreteType: Option[Type] = None

        ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
          if (fieldName == typeFieldName) {
            maybeConcreteType = f.map(_.apply(ops.unapplyString(fieldValueJson).get)).orElse {
              val DeserializationSuccess(TypeTagged(concreteType)) = typeDeserializer.deserialize(path \ fieldName, fieldValueJson)
              Some(concreteType)
            }
          }
        })

        maybeConcreteType match {
          case Some(concreteType) =>
            val populatedConcreteType = populateConcreteType(concreteType)
            val concreteDeserializer = context.deserializer(populatedConcreteType)
            concreteDeserializer.deserialize(path, json).asInstanceOf[DeserializationResult[T]]

          case None =>
            throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeFieldName"\n""" /* FIXME + reader.showError()*/ )
        }

      case JsonString(s) if (guidance.isMapKey) =>
        val deserializer = context.typeAdapterOf[T].deserializer
        deserializer.deserialize(Path.Root, JsonParser.parse(s).get)

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON object", reportedBy = self))
    }

}
