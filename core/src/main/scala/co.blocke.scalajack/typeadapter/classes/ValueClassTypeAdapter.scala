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
  def read[WIRE](path: Path, reader: Transceiver[WIRE]): DerivedValueClass =
    derive(sourceTypeAdapter.read(path, reader))
  def write[WIRE](t: DerivedValueClass, writer: Transceiver[WIRE], out: Builder[Any, WIRE], isMapKey: Boolean): Unit =
    sourceTypeAdapter.write(unwrap(t), writer, out, isMapKey)
}
/*
                                                               override val irTransceiver: IRTransceiver[DerivedValueClass]) extends TypeAdapter[DerivedValueClass]

class ValueClassIRTransceiver[Derived, Source](
                                                       sourceTransceiver: IRTransceiver[Source],
                                                       unwrap:            Derived => Source,
                                                       derive:            Source => Derived)(implicit derivedTypeTag: TypeTag[Derived], sourceTypeTag: TypeTag[Source]) extends IRTransceiver[Derived] {

  private val derivedType: Type = derivedTypeTag.tpe
  private val sourceType: Type = sourceTypeTag.tpe
  override def toString: String = s"DerivedValueClassIRTransceiver[${derivedTypeTag.tpe}, ${sourceTypeTag.tpe}]"

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Derived] =
    sourceTransceiver.read(path, ir) map {
      case TypeTagged(source) =>
        val derived = derive(source)
        TypeTagged(derived, derivedType)
    }

  override def write[IR, WIRE](tagged: TypeTagged[Derived])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(derived) =>
        val source = unwrap(derived)
        sourceTransceiver.write(TypeTagged(source, sourceType))
    }

}
*/ 