package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.reflect.runtime.universe.TermName

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

        val classMirror = reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        TupleTypeAdapter[T](new TupleDeserializer[T](fields, constructorMirror), new TupleSerializer[T](fields), fields.toList, constructorMirror).asInstanceOf[TypeAdapter[T]]

      case _ =>
        next.typeAdapterOf[T]
    }

}

case class TupleTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    fields:                    List[Field[T]],
    constructorMirror:         MethodMirror) extends TypeAdapter[T]
