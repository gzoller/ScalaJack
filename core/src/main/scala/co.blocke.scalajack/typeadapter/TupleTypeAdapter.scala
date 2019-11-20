package co.blocke.scalajack
package typeadapter

import model._
import util.Reflection
import java.lang.reflect.Method

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{
  ClassSymbol,
  MethodMirror,
  MethodSymbol,
  TermName,
  TypeTag
}
import TupleTypeAdapterFactory.TupleField

import scala.collection.mutable
import scala.util.matching.Regex

object TupleTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  case class TupleField[F](
      index:                     Int,
      valueTypeAdapter:          TypeAdapter[F],
      valueAccessorMethodSymbol: MethodSymbol,
      valueAccessorMethod:       Method) {

    def valueIn(tuple: Any): F =
      valueAccessorMethod.invoke(tuple).asInstanceOf[F]

    def read(parser: Parser): Any = valueTypeAdapter.read(parser)

    def write[WIRE, T](
        tuple:  T,
        writer: Writer[WIRE],
        out:    mutable.Builder[WIRE, WIRE]): Unit =
      valueTypeAdapter.write(valueIn(tuple), writer, out)
  }

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  override def typeAdapterOf[T](
      classSymbol: ClassSymbol,
      next:        TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    classSymbol.fullName match {
      case tupleFullName(numberOfFieldsAsString) =>
        val numberOfFields = numberOfFieldsAsString.toInt
        val fieldTypes = tt.tpe.dealias.typeArgs

        val fields = for (i <- 0 until numberOfFields) yield {
          val fieldType = fieldTypes(i)
          val fieldTypeAdapter = taCache.typeAdapter(fieldType) match {
            case opt: OptionTypeAdapter[_] => opt.convertNullToNone()
            case ta                        => ta
          }

          val valueAccessorMethodSymbol =
            tt.tpe.member(TermName(s"_${i + 1}")).asMethod
          val valueAccessorMethod =
            Reflection.methodToJava(valueAccessorMethodSymbol)
          TupleField(
            i,
            fieldTypeAdapter,
            valueAccessorMethodSymbol,
            valueAccessorMethod
          )
        }

        val classMirror = currentMirror.reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(
          classSymbol.primaryConstructor.asMethod
        )

        TupleTypeAdapter(fields.toList, constructorMirror)
          .asInstanceOf[TypeAdapter[T]]

      case _ =>
        next.typeAdapterOf[T]
    }

}

case class TupleTypeAdapter[T >: Null](
    fields:            List[TupleField[_]],
    constructorMirror: MethodMirror)
  extends TypeAdapter[T]
  with Collectionish {

  def read(parser: Parser): T =
    if (parser.peekForNull)
      null
    else
      constructorMirror.apply(parser.expectTuple(fields): _*).asInstanceOf[T]

  // Create functions that know how to self-write each field.  The actual writing of each element
  // is done in TupleField where the specific field type F is known.
  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    if (t == null)
      writer.writeNull(out)
    else
      writer.writeTuple(
        fields.map(
          field =>
            (w: Writer[WIRE], builder: mutable.Builder[WIRE, WIRE]) =>
              field.write(t, w, builder)
        ),
        out
      )
}
