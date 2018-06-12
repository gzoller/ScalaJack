package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type, TypeTag, typeOf }

object TupleTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  val tupleTypeConstructorsByArity: Map[Int, Type] = Map(
    2 -> typeOf[(_, _)].typeConstructor,
    3 -> typeOf[(_, _, _)].typeConstructor,
    4 -> typeOf[(_, _, _, _)].typeConstructor,
    5 -> typeOf[(_, _, _, _, _)].typeConstructor,
    6 -> typeOf[(_, _, _, _, _, _)].typeConstructor,
    7 -> typeOf[(_, _, _, _, _, _, _)].typeConstructor,
    8 -> typeOf[(_, _, _, _, _, _, _, _)].typeConstructor,
    9 -> typeOf[(_, _, _, _, _, _, _, _, _)].typeConstructor,
    10 -> typeOf[(_, _, _, _, _, _, _, _, _, _)].typeConstructor,
    11 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    12 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    13 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    14 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    15 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    16 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    17 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    18 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    19 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    20 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    21 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    22 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor)

  trait Field[Owner] {

    type Value

    val index: Int
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val valueAccessorMethodSymbol: MethodSymbol
    val valueAccessorMethod: Method

    def valueDeserializer: Deserializer[Value] = valueTypeAdapter.deserializer

    def valueSerializer: Serializer[Value] = valueTypeAdapter.serializer

    def valueIn(taggedTuple: TypeTagged[Owner]): TypeTagged[Value] =
      taggedTuple match {
        case TypeTagged(tuple) =>
          TypeTagged[Value](valueAccessorMethod.invoke(tuple).asInstanceOf[Value], valueType)
      }

    def read(reader: Reader): Any = {
      valueTypeAdapter.read(reader)
    }

    def write(fieldValue: TypeTagged[Value], writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(fieldValue.get, writer)
    }

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
            case vta: OptionTypeAdapter[_] => vta.noneAsNull
            case vta                       => vta
          }
          val fieldValueAccessorMethodSymbol = tt.tpe.member(TermName(s"_${i + 1}")).asMethod
          val fieldValueAccessorMethod = Reflection.methodToJava(fieldValueAccessorMethodSymbol)

          new Field[T] {
            override type Value = Any
            override val index: Int = i
            override val valueType: Type = fieldType
            override val valueTypeAdapter: TypeAdapter[Value] = fieldTypeAdapter.asInstanceOf[TypeAdapter[Value]]
            override val valueAccessorMethodSymbol: MethodSymbol = fieldValueAccessorMethodSymbol
            override val valueAccessorMethod: Method = fieldValueAccessorMethod
          }
        }

        val classMirror = currentMirror.reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        TupleTypeAdapter[T](fields.toList, constructorMirror).asInstanceOf[TypeAdapter[T]]

      case _ =>
        next.typeAdapterOf[T]
    }

}

case class TupleTypeAdapter[T](
    fields:            List[Field[T]],
    constructorMirror: MethodMirror) extends TypeAdapter[T] {

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.BeginArray =>
        val fieldValues = new Array[Any](fields.length)

        reader.beginArray()

        for (field <- fields) {
          val fieldValue = field.read(reader)
          fieldValues(field.index) = fieldValue
        }

        reader.endArray()

        constructorMirror.apply(fieldValues: _*).asInstanceOf[T]

      case TokenType.Null =>
        reader.readNull().asInstanceOf[T]
    }

  override def write(tuple: T, writer: Writer): Unit =
    if (tuple == null) {
      writer.writeNull()
    } else {
      writer.beginArray()

      for (field <- fields) {
        val fieldValue = field.valueIn(TypeTagged.inferFromRuntimeClass(tuple))
        field.write(fieldValue, writer)
      }

      writer.endArray()
    }

}
