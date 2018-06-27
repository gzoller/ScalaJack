package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val (parameter :: Nil) :: Nil = constructorSymbol.paramLists
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      //      type Source = Any
      //      type Derived = T

      type Derived = T
      val derivedTypeTag: TypeTag[Derived] = tt.asInstanceOf[TypeTag[Derived]]

      type Source = Any
      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val sourceTypeTag: TypeTag[Source] = TypeTags.of[Source](valueType)

      val valueTypeAdapter = context.typeAdapter(valueType).asInstanceOf[TypeAdapter[Source]]

      def wrap(source: Source): Derived = constructorMirror.apply(source).asInstanceOf[Derived]

      def unwrap(wrapped: Derived): Source = accessorMethod.invoke(wrapped)

      DerivedValueClassAdapter[Derived, Source](
        new DerivedValueClassDeserializer[Derived, Source](valueTypeAdapter.deserializer, wrap)(derivedTypeTag, sourceTypeTag),
        new DerivedValueClassSerializer[Derived, Source](unwrap, valueTypeAdapter.serializer)(sourceTypeTag),
        constructorMirror, accessorMethodSymbol, accessorMethod, valueTypeAdapter)
    } else {
      next.typeAdapterOf[T]
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    override val deserializer: Deserializer[DerivedValueClass],
    override val serializer:   Serializer[DerivedValueClass],
    constructorMirror:         MethodMirror,
    accessorMethodSymbol:      MethodSymbol,
    accessorMethod:            Method,
    valueTypeAdapter:          TypeAdapter[Value]) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val wrappedValue = accessorMethod.invoke(value).asInstanceOf[Value]
    valueTypeAdapter.write(wrappedValue, writer)
  }

}
