package co.blocke.scalajack
package typeadapter

import scala.collection.mutable

class TraitSerializer[T](
    typeFieldName:  MemberName, // hint label
    typeSerializer: Serializer[Type], // Ignored for modified versions, as it is set by concrete type
    f:              Option[BijectiveFunction[String, Type]] = None // optional string->type map (hint modifier)
)(implicit tt: TypeTag[T], context: Context) extends Serializer[T] {

  // f is None for global TraitSerializer.  It is set in ScalaJack for specific type modifiers set using withHindModifiers()

  private val polymorphicType: Type = tt.tpe
  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]
  private val TypeType: Type = typeOf[Type]
  private val stringSerializer = context.typeAdapterOf[String].serializer

  private def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(JsonNull())

      case TypeTagged(value) =>
        // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
        val concreteType = classSymbol(value.getClass).toType
        val populatedConcreteType = populateConcreteType(concreteType)
        val concreteSerializer = context.serializer(populatedConcreteType).asInstanceOf[Serializer[T]]

        concreteSerializer.serialize(tagged) map {
          case JsonObject(x) =>
            val concreteFields = x.asInstanceOf[ops.ObjectFields]

            val SerializationSuccess(typeJson) = f.map(bij =>
              stringSerializer.serialize(TypeTagged(bij.unapply(concreteType), TypeType))).getOrElse(
              typeSerializer.serialize(TypeTagged(concreteType, TypeType))
            )

            JsonObject { appendField =>
              appendField(typeFieldName, typeJson)
              ops.foreachObjectField(concreteFields, { (concreteFieldName, concreteFieldValue) =>
                appendField(concreteFieldName, concreteFieldValue)
              })
            }

          case json =>
            json
        }
    }

}
