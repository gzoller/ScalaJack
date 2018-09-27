package co.blocke.scalajack
package typeadapter

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isTrait) {
      val typeTypeAdapter = context.typeAdapterOf[Type]
      PolymorphicTypeAdapter(
        new PolymorphicDeserializer[T](hintFieldName, typeTypeAdapter.deserializer),
        new PolymorphicSerializer[T](hintFieldName, typeTypeAdapter.serializer, context),
        hintFieldName, typeTypeAdapter, context.typeAdapterOf[MemberName], context, tt.tpe)
    } else {
      next.typeAdapterOf[T]
    }

}

case class PolymorphicTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    typeMemberName:            MemberName,
    typeTypeAdapter:           TypeAdapter[Type],
    memberNameTypeAdapter:     TypeAdapter[MemberName],
    context:                   Context,
    polymorphicType:           Type) extends TypeAdapter[T] {

  import scala.collection.mutable

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))
}
