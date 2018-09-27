package co.blocke.scalajack
package typeadapter

import scala.collection.mutable

class TraitSerializer[T](
    typeFieldName:  MemberName, // hint label
    typeSerializer: Serializer[Type],
    context:        Context)(implicit tt: TypeTag[T]) extends Serializer[T] {

  private val polymorphicType: Type = tt.tpe
  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]
  private val TypeType: Type = typeOf[Type]

  private def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def serialize[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
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

            val SerializationSuccess(typeJson) = typeSerializer.serialize(TypeTagged(concreteType, TypeType))

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
