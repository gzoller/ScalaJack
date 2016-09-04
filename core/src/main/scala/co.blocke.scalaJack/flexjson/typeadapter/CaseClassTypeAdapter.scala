package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.typeadapter.CaseClassTypeAdapter.Parameter
import co.blocke.scalajack.flexjson.{ Context, EmptyReader, Reader, TypeAdapter, TypeAdapterFactory, Writer }

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type, typeOf }

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Parameter[T](
      index:              Int,
      name:               String,
      valueTypeAdapter:   TypeAdapter[T],
      accessor:           MethodSymbol,
      defaultValueMirror: Option[MethodMirror]
  ) {

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    def hasDefaultValue: Boolean = defaultValueMirror.isDefined

    def defaultValue: T = defaultValueMirror.get.apply().asInstanceOf[T]

  }

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type] = List.empty[Type]): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      println(tpe)
      println("HERE: " + constructorSymbol.infoIn(tpe))
      println("Super: " + superParamTypes)

      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val typeBeforeSubstitution = constructorSymbol.infoIn(tpe)
      println("BEFORE: " + typeBeforeSubstitution)

      val typeAfterSubstitution =
        if (superParamTypes.isEmpty) {
          println(":::1:::")
          typeBeforeSubstitution.substituteTypes(tpe.typeParams, tpe.typeParams.map(_ => typeOf[Any]))
        } else {
          println(":::2::: " + superParamTypes)
          typeBeforeSubstitution.substituteTypes(tpe.typeConstructor.typeParams, superParamTypes)
        }
      println("AFTER : " + typeAfterSubstitution)

      val parameters = typeAfterSubstitution.paramLists.flatten.zipWithIndex.map({
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

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      Some(CaseClassTypeAdapter(tpe, constructorMirror, parameters, memberNameTypeAdapter))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    parameters:            List[Parameter[_]],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends TypeAdapter[T] {

  val parametersByName = parameters.map(parameter ⇒ parameter.name → parameter.asInstanceOf[Parameter[Any]]).toMap

  override def read(reader: Reader): T = {
    val numberOfParameters = parameters.length

    val arguments = new Array[Any](numberOfParameters)
    val found = new mutable.BitSet(numberOfParameters)

    reader.beginObject()

    while (reader.hasMoreMembers) {
      val memberName = memberNameTypeAdapter.read(reader)

      val optionalParameter = parametersByName.get(memberName)
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

    for (parameter ← parameters) {
      val classTag = ClassTag[T](value.getClass)
      val instanceMirror = currentMirror.reflect(value)(classTag)
      val accessorMirror = instanceMirror.reflectMethod(parameter.accessor)
      val parameterValue = accessorMirror.apply()

      memberNameTypeAdapter.write(parameter.name, writer)
      parameter.writeValue(parameterValue, writer)
    }

    writer.endObject()
  }

}
