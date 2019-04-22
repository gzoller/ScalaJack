package co.blocke.scalajack
package typeadapter
package classes

import model._
import util.{ Path, Reflection }

import scala.collection.mutable.Builder

object ValueClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isDerivedValueClass) {
      val tpe = tt.tpe

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val (parameter :: Nil) :: Nil = constructorSymbol.paramLists
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      type Derived = T
      type Source = Any
      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val valueTypeAdapter = context.typeAdapter(valueType).asInstanceOf[TypeAdapter[Source]]

      def wrap(source: Source): Derived = constructorMirror.apply(source).asInstanceOf[Derived]
      def unwrap(wrapped: Derived): Source = accessorMethod.invoke(wrapped)

      ValueClassAdapter[Derived, Source](valueTypeAdapter, unwrap, wrap)
    } else {
      next.typeAdapterOf[T]
    }
}

case class ValueClassAdapter[DerivedValueClass, Value](
    sourceTypeAdapter: TypeAdapter[Value],
    unwrap:            DerivedValueClass => Value,
    derive:            Value => DerivedValueClass) extends TypeAdapter[DerivedValueClass] {
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): DerivedValueClass =
    derive(sourceTypeAdapter.read(path, reader))
  def write[WIRE](t: DerivedValueClass, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    sourceTypeAdapter.write(unwrap(t), writer, out, isMapKey)
}
