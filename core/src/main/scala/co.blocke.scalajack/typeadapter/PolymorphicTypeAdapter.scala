package co.blocke.scalajack
package typeadapter

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isTrait) {
      PolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context, tt.tpe)
    } else {
      next.typeAdapterOf[T]
    }

}

class PolymorphicWriter(
    override val delegate: Writer,
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

    if (depth == 1) {
      memberNameTypeAdapter.write(typeFieldName, this)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

case class PolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context,
    polymorphicType:       Type) extends TypeAdapter[T] {

  import scala.collection.mutable

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def read(reader: Reader): T = {
    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[T]
    } else {
      val savedPosition = reader.position

      reader.beginObject()

      var optionalConcreteType: Option[Type] = None

      while (optionalConcreteType.isEmpty && reader.hasMoreMembers) {
        val memberName = memberNameTypeAdapter.read(reader)

        if (memberName == typeMemberName) {
          val concreteType = typeTypeAdapter.read(reader)
          optionalConcreteType = Some(concreteType)
        } else {
          reader.skipValue()
        }
      }

      val concreteType = optionalConcreteType.getOrElse(throw new java.lang.IllegalStateException(s"""Could not find type field named "$typeMemberName"\n""" + reader.showError()))
      val populatedConcreteType = populateConcreteType(concreteType)
      val concreteTypeAdapter = context.typeAdapter(populatedConcreteType)

      reader.position = savedPosition

      concreteTypeAdapter.read(reader).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit = {
    // TODO figure out a better way to infer the type (perhaps infer the type arguments?)
    if (value == null) {
      writer.writeNull()
    } else {
      val concreteType = classSymbol(value.getClass).toType
      val populatedConcreteType = populateConcreteType(concreteType)
      val valueTypeAdapter = context.typeAdapter(populatedConcreteType).asInstanceOf[TypeAdapter[T]]

      val polymorphicWriter = new PolymorphicWriter(writer, typeMemberName, populatedConcreteType, typeTypeAdapter, memberNameTypeAdapter)
      valueTypeAdapter.write(value, polymorphicWriter)
    }
  }

}
