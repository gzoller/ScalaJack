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

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[T] =
    ast match {
      case AstNull() =>
        DeserializationSuccess(nullTypeTagged)

      case AstObject(x) =>
        val fields = x.asInstanceOf[ops.ObjectFields]

        var maybeConcreteType: Option[Type] = None

        ops.foreachObjectField(fields, { (fieldName, fieldValueAst) =>
          if (fieldName == typeFieldName) {
            maybeConcreteType = f.map(_.apply(ops.unapplyString(fieldValueAst).get)).orElse {
              val DeserializationSuccess(TypeTagged(concreteType)) = typeDeserializer.deserialize(path \ fieldName, fieldValueAst)
              Some(concreteType)
            }
          }
        })

        maybeConcreteType match {
          case Some(concreteType) =>
            val populatedConcreteType = populateConcreteType(concreteType)
            val concreteDeserializer = context.deserializer(populatedConcreteType)
            concreteDeserializer.deserialize(path, ast).asInstanceOf[DeserializationResult[T]]

          case None =>
            throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeFieldName"\n""" /* FIXME + reader.showError()*/ )
        }

      case AstString(s) if (guidance.isMapKey) =>
        val deserializer = context.typeAdapterOf[T].deserializer
        deserializer.deserialize(Path.Root, ops.parse(s.asInstanceOf[S]))

      case _ =>
        DeserializationFailure(path, DeserializationError.Unexpected("Expected a JSON object", reportedBy = self))
    }

}
