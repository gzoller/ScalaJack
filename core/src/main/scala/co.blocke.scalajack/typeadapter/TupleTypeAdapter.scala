package co.blocke.scalajack
package typeadapter

import model._
import util.{ Path, Reflection }
import java.lang.reflect.Method

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, TypeTag }
import TupleTypeAdapterFactory.TupleField

import scala.collection.mutable.Builder

object TupleTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  case class TupleField[F](
      index:                     Int,
      valueTypeAdapter:          TypeAdapter[F],
      valueAccessorMethodSymbol: MethodSymbol,
      valueAccessorMethod:       Method) {

    def valueIn(tuple: Any): F = valueAccessorMethod.invoke(tuple).asInstanceOf[F]

    def read[WIRE](path: Path, reader: Reader[WIRE]): Any =
      valueTypeAdapter.read(path, reader)

    def write[WIRE, T](tuple: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
      valueTypeAdapter.write(valueIn(tuple), writer, out, isMapKey)
  }

  val tupleFullName = """scala.Tuple(\d+)""".r

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    classSymbol.fullName match {
      case tupleFullName(numberOfFieldsAsString) =>
        val numberOfFields = numberOfFieldsAsString.toInt
        val fieldTypes = tt.tpe.dealias.typeArgs

        val fields = for (i <- 0 until numberOfFields) yield {
          val fieldType = fieldTypes(i)
          val fieldTypeAdapter = context.typeAdapter(fieldType) match {
            case opt: OptionTypeAdapter[_] => opt.convertNullToNone()
            case ta                        => ta
          }

          val valueAccessorMethodSymbol = tt.tpe.member(TermName(s"_${i + 1}")).asMethod
          val valueAccessorMethod = Reflection.methodToJava(valueAccessorMethodSymbol)
          TupleField(i, fieldTypeAdapter, valueAccessorMethodSymbol, valueAccessorMethod)
        }

        val classMirror = currentMirror.reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        TupleTypeAdapter(fields.toList, constructorMirror).asInstanceOf[TypeAdapter[T]]

      case _ =>
        next.typeAdapterOf[T]
    }

}

case class TupleTypeAdapter[T >: Null](
    fields:            List[TupleField[_]],
    constructorMirror: MethodMirror) extends TypeAdapter[T] with Collectionish {

  def read[WIRE](path: Path, reader: Reader[WIRE]): T =
    reader.head.tokenType match {
      case TokenType.Null =>
        reader.next
        null
      case _ =>
        constructorMirror.apply(reader.readTuple(path, fields): _*).asInstanceOf[T]
    }

  // Create functions that know how to self-write each field.  The actual writing of each element
  // is done in TupleField where the specific field type F is known.
  def write[WIRE](t: T, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    if (t == null)
      writer.writeNull(out)
    else
      writer.writeTuple(
        fields.map(field => (w: Writer[WIRE], builder: Builder[WIRE, WIRE]) => field.write(t, w, builder, isMapKey)),
        out)
}
