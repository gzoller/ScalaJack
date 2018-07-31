package co.blocke.scalajack
package csv

import java.lang.reflect.Method

import co.blocke.scalajack.csv.CSVCaseClassTypeAdapter.Member
import co.blocke.scalajack.typeadapter.OptionTypeAdapter

import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type, TypeTag }

object CSVCaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[T](
      index:                              Int,
      name:                               String,
      valueTypeAdapter:                   TypeAdapter[T],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]]
  ) {

    val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    def valueIn(instance: Any): T = {
      val value = valueAccessorMethod.invoke(instance)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[T]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) =>
            methodMirror.apply(value).asInstanceOf[T]

          case None =>
            value.asInstanceOf[T]
        }
      }
    }

    def writeValue(parameterValue: Any, writer: Writer): Unit =
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)

  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val members = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten.zipWithIndex.map({
        case (member, index) =>
          val memberName = member.name.encodedName.toString
          val accessorMethodSymbol = tt.tpe.member(TermName(memberName)).asMethod
          val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

          val (derivedValueClassConstructorMirror, memberClass) =
            if (member.typeSignature.typeSymbol.isClass) {
              val memberClassSymbol = member.typeSignature.typeSymbol.asClass

              if (memberClassSymbol.isDerivedValueClass) {
                val memberClass = currentMirror.runtimeClass(memberClassSymbol)
                // The accessor will actually return the "inner" value, not the value class.
                val constructorMethodSymbol = memberClassSymbol.primaryConstructor.asMethod
                //              val innerClass = currentMirror.runtimeClass(constructorMethodSymbol.paramLists.flatten.head.info.typeSymbol.asClass)
                (Some(currentMirror.reflectClass(memberClassSymbol).reflectConstructor(constructorMethodSymbol)), Some(memberClass))
              } else {
                (None, None)
              }
            } else {
              (None, None)
            }

          val memberType = member.asTerm.typeSignature
          val memberTypeAdapter = context.typeAdapter(memberType)
          Member(index, memberName, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, memberClass)
      })

      CSVCaseClassTypeAdapter(tt.tpe, constructorMirror, tt.tpe, members).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class CSVCaseClassTypeAdapter[T >: Null](
    caseClassType:     Type,
    constructorMirror: MethodMirror,
    tpe:               Type,
    members:           List[Member[_]]
) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    reader.peek match {
      case TokenType.Null =>
        reader.readNull()

      case TokenType.BeginObject =>
        reader.beginObject()
        val arguments = members.map(_.valueTypeAdapter.read(reader))
        reader.endObject()
        constructorMirror.apply(arguments: _*).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member <- members) {
        // We don't write member names for CSV
        member.writeValue(member.valueIn(value), writer)
      }
      writer.endObject()
    }

}
