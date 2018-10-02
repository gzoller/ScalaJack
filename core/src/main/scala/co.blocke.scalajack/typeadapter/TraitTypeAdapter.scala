package co.blocke.scalajack
package typeadapter

// This should come *after* SealedTraitTypeAdapter in the context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
case class TraitTypeAdapterFactory(hintLabel: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isTrait) {
      val typeTypeAdapter = context.typeAdapterOf[Type]
      TraitTypeAdapter(
        new TraitDeserializer[T](hintLabel, typeTypeAdapter.deserializer),
        new TraitSerializer[T](hintLabel, typeTypeAdapter.serializer, context),
        hintLabel, typeTypeAdapter, context.typeAdapterOf[MemberName], context, tt.tpe)
    } else {
      next.typeAdapterOf[T]
    }

}

case class TraitTypeAdapter[T](
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
