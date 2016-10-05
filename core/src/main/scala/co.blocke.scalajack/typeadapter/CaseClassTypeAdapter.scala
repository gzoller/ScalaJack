package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import scala.language.{existentials, reflectiveCalls}
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{Annotation, ClassSymbol, MethodMirror, MethodSymbol, TermName, Type}

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member(
      index:                              Int,
      name:                               String,
      annotations:                        List[Annotation],
      valueTypeAdapter:                   TypeAdapter[Any],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]]
  ) extends ClassLikeTypeAdapter.Member {

    override type OwnerType = Any
    override type MemberValueType = Any

    def valueIn(owner: OwnerType): MemberValueType = {
      val value = valueAccessorMethod.invoke(owner)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[MemberValueType]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) ⇒
            methodMirror.apply(value)

          case None ⇒
            value.asInstanceOf[MemberValueType]
        }
      }
    }

    def defaultValue: Option[MemberValueType] = defaultValueMirror.map(_.apply()).orElse(valueTypeAdapter.defaultValue)

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
          val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
          Member(index, memberName, accessorMethodSymbol.annotations, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass)
      })

      Some(CaseClassTypeAdapter(tpe, constructorMirror, memberNameTypeAdapter, members))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[CaseClassTypeAdapter.Member]
) extends ClassLikeTypeAdapter[T] {

  override def instantiate(memberValues: Array[Any]): T = {
    constructorMirror.apply(memberValues: _*).asInstanceOf[T]
  }

}
