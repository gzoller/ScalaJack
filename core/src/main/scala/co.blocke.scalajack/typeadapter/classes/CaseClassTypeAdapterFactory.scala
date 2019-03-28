package co.blocke.scalajack
package typeadapter
package classes

import model._
import util.Reflection

import scala.collection.immutable.ListMap
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

object CaseClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val clazz = currentMirror.runtimeClass(classSymbol)
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val isSJCapture = !(tt.tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap = tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap

      val params1 = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten
      val params2 = constructorSymbol.typeSignatureIn(tt.tpe.typeSymbol.asType.toType).paramLists.flatten

      val fieldMembers = for (((member, param2), index) <- (params1 zip params2).zipWithIndex) yield {
        val memberName = member.name.encodedName.toString
        val accessorMethod = Reflection.methodToJava(tt.tpe.member(TermName(memberName)).asMethod)

        val (derivedValueClassConstructorMirror, memberClass) =
          if (member.typeSignature.typeSymbol.isClass) {
            val memberClassSymbol = member.typeSignature.typeSymbol.asClass

            if (memberClassSymbol.isDerivedValueClass) {
              val memberClass = currentMirror.runtimeClass(memberClassSymbol)
              // The accessor will actually return the "inner" value, not the value class.
              val constructorMethodSymbol = memberClassSymbol.primaryConstructor.asMethod
              (Some(currentMirror.reflectClass(memberClassSymbol).reflectConstructor(constructorMethodSymbol)), Some(memberClass))
            } else {
              (None, None)
            }
          } else {
            (None, None)
          }

        val memberType = member.asTerm.typeSignature

        val declaredMemberType = param2.asTerm.typeSignature

        // Exctract DBKey annotation if present
        val optionalDbKeyIndex = ClassHelper.getAnnotationValue[DBKey, Int](member, Some(0))

        // Extract MapName annotation if present
        val optionalMapName = ClassHelper.getAnnotationValue[MapName, String](member)

        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        (optionalMapName.getOrElse(memberName), ClassHelper.ClassFieldMember[T, Any](
          index,
          optionalMapName.getOrElse(memberName),
          memberType,
          memberTypeAdapter,
          declaredMemberType,
          accessorMethod,
          derivedValueClassConstructorMirror,
          ClassHelper.extractDefaultConstructorParamValueMethod(clazz, index + 1),
          memberClass,
          optionalDbKeyIndex,
          optionalMapName,
          tt.tpe,
          None,
          None))
      }
      val orderdFieldMembers = ListMap(fieldMembers: _*)

      // All type members found for class.  (Some may be removed if not used for constructor)
      val justDeclaredFieldTypes = orderdFieldMembers.map(_._2.declaredValueType).toList
      val typeMembers = tm collect {
        case m if typeIsUsed(m.typeSignature, justDeclaredFieldTypes) =>
          ClassHelper.TypeMember[T](m.name.decodedName.toString, m.typeSignature, classTypeParamMap(m.typeSignature.typeSymbol))
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = ClassHelper.getAnnotationValue[Collection, String](classSymbol)

      CaseClassTypeAdapter(
        classSymbol.name.toString(),
        typeMembers.map(typeMember => typeMember.name -> typeMember).toMap,
        orderdFieldMembers,
        constructorMirror,
        isSJCapture,
        collectionAnnotation)
    } else {
      next.typeAdapterOf[T]
    }

  @inline private def typeIsUsed(typeParam: Type, fields: List[Type]): Boolean = {
    // Simple use (first-level)
    fields.exists(_ == typeParam) ||
      // Used as a parameter of a field member
      fields.foldRight(false) { case (f, acc) => acc || typeIsUsed(typeParam, f.typeArgs) }
  }
}
