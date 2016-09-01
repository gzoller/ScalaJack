package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.typeadapter.TupleTypeAdapter.TupleField
import co.blocke.scalajack.flexjson.{ Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.language.existentials
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type }

object TupleTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class TupleField(
      fieldValueTypeAdapter:    TypeAdapter[_],
      fieldValueAccessorSymbol: MethodSymbol
  ) {

    def read(reader: Reader): Any = {
      fieldValueTypeAdapter.read(reader)
    }

    def write(fieldValue: Any, writer: Writer): Unit = {
      fieldValueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(fieldValue, writer)
    }

  }

  val tupleFullName = """scala.Tuple(\d+)""".r

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    classSymbol.fullName match {
      case tupleFullName(numberOfFieldsAsString) ⇒
        val numberOfFields = numberOfFieldsAsString.toInt
        val fieldTypes = tpe.dealias.typeArgs

        val fields = for (i ← 0 until numberOfFields) yield {
          val fieldType = fieldTypes(i)
          val fieldTypeAdapter = context.typeAdapter(fieldType)
          val fieldAccessorSymbol = tpe.member(TermName(s"_${i + 1}")).asMethod
          TupleField(fieldTypeAdapter, fieldAccessorSymbol)
        }

        val classMirror = currentMirror.reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        Some(TupleTypeAdapter(fields.toList, constructorMirror))

      case _ ⇒
        None
    }

}

case class TupleTypeAdapter[T](
    fields:            List[TupleField],
    constructorMirror: MethodMirror
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    reader.beginArray()

    val numberOfFields = fields.length

    val fieldValues = new Array[Any](numberOfFields)

    var i = 0
    for (field ← fields) {
      val fieldValue = field.read(reader)
      fieldValues(i) = fieldValue
      i += 1
    }

    reader.endArray()

    constructorMirror.apply(fieldValues: _*).asInstanceOf[T]
  }

  override def write(tuple: T, writer: Writer): Unit = {
    writer.beginArray()

    val tupleMirror = currentMirror.reflect(tuple)(ClassTag(tuple.getClass))

    for (field ← fields) {
      val fieldValueAccessorMirror = tupleMirror.reflectMethod(field.fieldValueAccessorSymbol)
      val fieldValue = fieldValueAccessorMirror.apply()
      field.write(fieldValue, writer)
    }

    writer.endArray()
  }

}
