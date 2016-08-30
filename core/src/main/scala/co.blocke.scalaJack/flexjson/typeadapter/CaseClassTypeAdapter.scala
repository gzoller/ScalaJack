package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.typeadapter.CaseClassTypeAdapter.Parameter
import co.blocke.scalajack.flexjson.{Context, EmptyReader, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ClassSymbol, MethodMirror, MethodSymbol, TermName, Type}

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Parameter[T](index: Int,
                          name: String,
                          valueTypeAdapter: TypeAdapter[T],
                          accessor: MethodSymbol,
                          defaultValueMirror: Option[MethodMirror])

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val parameters = constructorSymbol.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs).paramLists.flatten.zipWithIndex.map({
        case (param, index) ⇒
          val parameterName = param.name.encodedName.toString
          val accessor = tpe.member(TermName(parameterName)).asMethod

          val defaultValueAccessor = companionType.member(TermName("apply$default$" + (index + 1)))
          val defaultValueAccessorMirror =
            if (defaultValueAccessor.isMethod) {
              Some(companionMirror.reflectMethod(defaultValueAccessor.asMethod))
            } else {
              None
            }

          Parameter(index, parameterName, context.typeAdapter(param.info), accessor, defaultValueAccessorMirror)
      })


      Some(CaseClassTypeAdapter(constructorMirror, parameters))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T](constructorMirror: MethodMirror,
                                   parameters: List[Parameter[_]]) extends TypeAdapter[T] {

  val parametersByName = parameters.map(parameter ⇒ parameter.name → parameter.asInstanceOf[Parameter[Any]]).toMap

  override def read(reader: Reader): T = {
    val numberOfParameters = parameters.length

    val arguments = new Array[Any](numberOfParameters)
    val found = new mutable.BitSet(numberOfParameters)

    reader.beginObject()

    while (reader.hasMoreFields) {
      val name = reader.readIdentifier()

      val optionalParameter = parametersByName.get(name)
      optionalParameter match {
        case Some(parameter) ⇒
          arguments(parameter.index) = parameter.valueTypeAdapter.read(reader)
          found(parameter.index) = true

        case None ⇒
          reader.skipValue()
      }
    }

    var index = 0
    while (index < numberOfParameters) {
      if (!found(index)) {
        val parameter = parameters(index)
        parameter.defaultValueMirror match {
          case Some(mirror) ⇒
            arguments(index) = mirror.apply()

          case None ⇒
            arguments(index) = parameter.valueTypeAdapter.read(EmptyReader)
        }
      }

      index += 1
    }

    reader.endObject()

    constructorMirror.apply(arguments: _*).asInstanceOf[T]
  }

  override def write(value: T, writer: Writer): Unit = {
    writer.beginObject()

    var isFirstParameter = true

    for (parameter ← parameters) {
      val parameterValue = currentMirror.reflect(value)(ClassTag(value.getClass)).reflectMethod(parameter.accessor).apply()

      if (isFirstParameter) {
        isFirstParameter = false
      } else {
        writer.writeValueSeparator()
      }

      writer.writeName(parameter.name)
      writer.writeNameSeparator()
      parameter.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    writer.endObject()
//    currentMirror.reflect(value).reflectMethod()
  }

}
