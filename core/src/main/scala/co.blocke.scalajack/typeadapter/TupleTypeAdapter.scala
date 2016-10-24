package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, TypeTag }

object TupleTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Field[T](
      index:                     Int,
      valueTypeAdapter:          TypeAdapter[T],
      valueAccessorMethodSymbol: MethodSymbol,
      valueAccessorMethod:       Method
  ) {

    def valueIn(tuple: Any): T = {
      valueAccessorMethod.invoke(tuple).asInstanceOf[T]
    }

    def read(reader: Reader): Any = {
      valueTypeAdapter.read(reader)
    }

    def write(fieldValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(fieldValue, writer)
    }

  }

  val tupleFullName = """scala.Tuple(\d+)""".r

  override def typeAdapterOf[T](classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
    classSymbol.fullName match {
      case tupleFullName(numberOfFieldsAsString) ⇒
        val numberOfFields = numberOfFieldsAsString.toInt
        val fieldTypes = tt.tpe.dealias.typeArgs

        val fields = for (i ← 0 until numberOfFields) yield {
          val fieldType = fieldTypes(i)
          val fieldTypeAdapter = context.typeAdapter(fieldType) match {
            case vta: OptionTypeAdapter[_] ⇒ vta.nullVersion
            case vta                       ⇒ vta
          }
          val valueAccessorMethodSymbol = tt.tpe.member(TermName(s"_${i + 1}")).asMethod
          val valueAccessorMethod = Reflection.methodToJava(valueAccessorMethodSymbol)
          Field(i, fieldTypeAdapter, valueAccessorMethodSymbol, valueAccessorMethod)
        }

        val classMirror = currentMirror.reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        TupleTypeAdapter(fields.toList, constructorMirror).asInstanceOf[TypeAdapter[T]]

      case _ ⇒
        next.typeAdapterOf[T](context)
    }

}

case class TupleTypeAdapter[T >: Null](
    fields:            List[Field[_]],
    constructorMirror: MethodMirror
) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.BeginArray ⇒
        val fieldValues = new Array[Any](fields.length)

        reader.beginArray()

        for (field ← fields) {
          val fieldValue = field.read(reader)
          fieldValues(field.index) = fieldValue
        }

        reader.endArray()

        constructorMirror.apply(fieldValues: _*).asInstanceOf[T]

      case TokenType.Null ⇒
        reader.readNull()
    }

  override def write(tuple: T, writer: Writer): Unit =
    if (tuple == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (field ← fields) {
        val fieldValue = field.valueIn(tuple)
        field.write(fieldValue, writer)
      }

      writer.endArray()
    }

}
