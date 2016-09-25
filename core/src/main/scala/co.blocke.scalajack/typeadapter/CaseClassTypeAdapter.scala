package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method
import java.nio.file.{Files, Paths, StandardOpenOption}

import CaseClassTypeAdapter.Member
import co.blocke.scalajack.bytecode.{BytecodeGenerator, MethodGenerator}

import scala.collection.mutable
import scala.language.{existentials, reflectiveCalls}
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ClassSymbol, MethodMirror, MethodSymbol, TermName, Type}

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[T](
      index:                              Int,
      name:                               String,
      valueType:                          Type,
      valueTypeAdapter:                   TypeAdapter[T],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]]
  ) {

    def valueIn(instance: Any): T = {
      val value = valueAccessorMethod.invoke(instance)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[T]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) ⇒
            methodMirror.apply(value).asInstanceOf[T]

          case None ⇒
            value.asInstanceOf[T]
        }
      }
    }

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    def defaultValue: T =
      defaultValueMirror match {
        case Some(mirror) ⇒
          mirror.apply().asInstanceOf[T]

        case None ⇒
          valueTypeAdapter.read(EmptyReader)
      }

  }

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      val members = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
        case (member, index) ⇒
          val memberName = member.name.encodedName.toString
          val accessorMethodSymbol = tpe.member(TermName(memberName)).asMethod
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

          val defaultValueAccessorMirror =
            if (member.typeSignature.typeSymbol.isClass) {
              val defaultValueAccessor = companionType.member(TermName("apply$default$" + (index + 1)))
              if (defaultValueAccessor.isMethod) {
                Some(companionMirror.reflectMethod(defaultValueAccessor.asMethod))
              } else {
                None
              }
            } else {
              None
            }

          val memberType = member.asTerm.typeSignature
          val memberTypeAdapter = context.typeAdapter(memberType)
          Member(index, memberName, memberType, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass)
      })

      val caseClassTypeAdapter = CaseClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members)
      Some(CaseClassTypeAdapterOptimizer.optimize(caseClassTypeAdapter))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[Member[_]]
) extends TypeAdapter[T] {

  val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[Member[Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        val numberOfMembers = members.length

        val arguments = new Array[Any](numberOfMembers)
        val found = new mutable.BitSet(numberOfMembers)

        reader.beginObject()

        while (reader.hasMoreMembers) {
          val memberName = memberNameTypeAdapter.read(reader)

          val optionalMember = membersByName.get(memberName)
          optionalMember match {
            case Some(member) ⇒
              arguments(member.index) = member.valueTypeAdapter.read(reader)
              found(member.index) = true

            case None ⇒
              reader.skipValue()
          }
        }

        reader.endObject()

        for (member ← members if !found(member.index)) {
          arguments(member.index) = member.defaultValue
        }

        constructorMirror.apply(arguments: _*).asInstanceOf[T]
    }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member ← members) {
        val memberValue = member.valueIn(value)

        memberNameTypeAdapter.write(member.name, writer)
        member.writeValue(memberValue, writer)
      }

      writer.endObject()
    }

}
