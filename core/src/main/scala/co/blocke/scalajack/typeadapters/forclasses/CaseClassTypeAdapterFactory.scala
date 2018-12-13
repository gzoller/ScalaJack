package co.blocke.scalajack
package typeadapters
package forclasses

import model._
import util.Reflection

import scala.collection.Map
import scala.reflect.runtime.universe._
import scala.language.existentials
import scala.reflect.runtime.currentMirror

object CaseClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val isSJCapture = !(tt.tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap = tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap
      val typeMembers = tm map { m =>
        ClassHelpers.TypeMember[T](m.name.decodedName.toString, m.typeSignature, classTypeParamMap(m.typeSignature.typeSymbol))
      }

      val params1 = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten
      val params2 = constructorSymbol.typeSignatureIn(tt.tpe.typeSymbol.asType.toType).paramLists.flatten

      val fieldMembers = for (((member, param2), index) <- (params1 zip params2).zipWithIndex) yield {
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

        val declaredMemberType = param2.asTerm.typeSignature

        // Exctract DBKey annotation if present
        val optionalDbKeyIndex = ClassHelpers.getAnnotationValue[DBKey, Int](member, Some(0))

        // Extract MapName annotation if present
        val optionalMapName = ClassHelpers.getAnnotationValue[MapName, String](member)

        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        ClassHelpers.ClassFieldMember[T, Any](index, optionalMapName.getOrElse(memberName), memberType, memberTypeAdapter, declaredMemberType, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass, optionalDbKeyIndex, optionalMapName, member.annotations)
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = ClassHelpers.getAnnotationValue[Collection, String](classSymbol)

      //      val ccTransceiver = CaseClassIRTransceiver(
      //        context,
      //        constructorMirror,
      //        context.typeAdapterOf[Type].irTransceiver,
      //        typeMembers,
      //        fieldMembers,
      //        isSJCapture,
      //        tt)

      //      CaseClassTypeAdapter[T](
      //        ccTransceiver,
      //        typeMembers,
      //        fieldMembers,
      //        collectionAnnotation)

      CaseClassTypeAdapter[T](
        typeMembers,
        fieldMembers.toArray,
        fieldMembers.map(fieldMember => fieldMember.name -> fieldMember).toMap,
        constructorMirror
      )
    } else {
      next.typeAdapterOf[T]
    }

}

//case class CaseClassTypeAdapter[T](
//    override val irTransceiver: IRTransceiver[T],
//    typeMembers:                List[ClassLikeTypeAdapter.TypeMember[T]],
//    fieldMembers:               List[ClassLikeTypeAdapter.FieldMember[T]],
//    collectionName:             Option[String]                            = None) extends ClassLikeTypeAdapter[T] {
//}