package co.blocke.scalajack
package typeadapter

import scala.collection.mutable

class PolymorphicDeserializer[T](
    typeFieldName:    MemberName,
    typeDeserializer: Deserializer[Type])(implicit tt: TypeTag[T], context: Context) extends Deserializer[T] {

  self =>

  private val polymorphicType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[T] = TypeTagged[T](null.asInstanceOf[T], polymorphicType)

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
    json match {
      case JsonNull() =>
        DeserializationSuccess(nullTypeTagged)

      case JsonObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        var maybeConcreteType: Option[Type] = None

        ops.foreachObjectField(fields, { (fieldName, fieldValueJson) =>
          if (fieldName == typeFieldName) {
            val DeserializationSuccess(TypeTagged(concreteType)) = typeDeserializer.deserialize(path \ fieldName, fieldValueJson)
            maybeConcreteType = Some(concreteType)
          }
        })

        maybeConcreteType match {
          case Some(concreteType) =>
            val populatedConcreteType = populateConcreteType(concreteType)
            val concreteDeserializer = context.deserializer(populatedConcreteType)
            concreteDeserializer.deserialize(path, json).asInstanceOf[DeserializationResult[T]]

          case None =>
            throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeFieldName"\n""" /* FIXME + reader.showError()*/ )
          //            val deserializer = TraitDeserializer[T]()
          //            deserializer.deserialize(path, json)
        }

      //        val concreteType = maybeConcreteType.getOrElse(throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeFieldName"\n""" /* FIXME + reader.showError()*/ ))
      //        val populatedConcreteType = populateConcreteType(concreteType)
      //        val concreteDeserializer = context.deserializer(populatedConcreteType)
      //
      //        concreteDeserializer.deserialize(path, json).asInstanceOf[DeserializationResult[T]]

      case _ =>
        DeserializationFailure(path, DeserializationError.Unsupported("Expected a JSON object", reportedBy = self))
    }

}
