package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.typeadapter.CaseClassTypeAdapter.Parameter
import co.blocke.scalajack.flexjson.{Context, EmptyReader, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.{MethodMirror, Type}
import scala.reflect.runtime.currentMirror

object CaseClassTypeAdapter extends TypeAdapterFactory {

  case class Parameter[T](index: Int, name: String, valueTypeAdapter: TypeAdapter[T])

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
    if (tpe.typeSymbol.isClass) {
      val classSymbol = tpe.typeSymbol.asClass
      if (classSymbol.isCaseClass) {
        val constructorSymbol = classSymbol.primaryConstructor.asMethod

        val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

        val parameters = constructorSymbol.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs).paramLists.flatten.zipWithIndex.map({
          case (param, index) ⇒
            Parameter(index, param.name.encodedName.toString, context.typeAdapter(param.info))
        })

        Some(CaseClassTypeAdapter(constructorMirror, parameters))
      } else {
        None
      }
    } else {
      None
    }
  }

}

case class CaseClassTypeAdapter[T](constructorMirror: MethodMirror,
                                   parameters: List[Parameter[_]]) extends TypeAdapter[T] {

  val parametersByName = parameters.map(parameter ⇒ parameter.name → parameter.asInstanceOf[Parameter[Any]]).toMap

  override def read(reader: Reader): T = {
    val numberOfParameters = parameters.length

    val arguments = new Array[Any](numberOfParameters)
    val found = new Array[Boolean](numberOfParameters)

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
        arguments(index) = parameters(index).valueTypeAdapter.read(EmptyReader)
      }

      index += 1
    }

    reader.endObject()

    constructorMirror.apply(arguments: _*).asInstanceOf[T]
  }

  override def write(value: T, writer: Writer): Unit = ???

}
