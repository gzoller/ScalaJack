package co.blocke.scalajack
package typeadapter

import model._
import util.Reflection
import scala.reflect.runtime.universe._

import scala.collection.mutable

object ValueClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](
      classSymbol: ClassSymbol,
      next:        TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror =
        reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val parameter = constructorSymbol.paramLists.head.head
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      type Derived = T
      type Source = Any
      val valueType = parameter
        .infoIn(tpe)
        .substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val valueTypeAdapter =
        taCache.typeAdapter(valueType).asInstanceOf[TypeAdapter[Source]]

      def wrap(source: Source): Derived =
        constructorMirror.apply(source).asInstanceOf[Derived]
      def unwrap(wrapped: Derived): Source = accessorMethod.invoke(wrapped)

      ValueClassTypeAdapter[Derived, Source](valueTypeAdapter, unwrap, wrap)
    } else {
      next.typeAdapterOf[T]
    }
}

case class ValueClassTypeAdapter[DerivedValueClass, Value](
    sourceTypeAdapter: TypeAdapter[Value],
    unwrap:            DerivedValueClass => Value,
    derive:            Value => DerivedValueClass
) extends TypeAdapter[DerivedValueClass] {
  def read(parser: Parser): DerivedValueClass =
    derive(sourceTypeAdapter.read(parser))
  def write[WIRE](
      t:      DerivedValueClass,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    sourceTypeAdapter.write(unwrap(t), writer, out)
}
