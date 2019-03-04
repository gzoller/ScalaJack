package co.blocke.scalajack
package typeadapter
package classes

import model.{ ClassHelper, _ }
import util.Reflection
import java.beans.{ Introspector, PropertyDescriptor }

import ClassHelper._

import scala.collection.immutable.{ List, ListMap }
import scala.language.existentials
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val tpe = tt.tpe
    if (classSymbol.isClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val clazz = currentMirror.runtimeClass(classSymbol)

      // For Java classes
      val maybeClass = currentMirror.runtimeClass(typeOf[Maybe].typeSymbol.asClass)

      // Exctract Collection name annotation if present
      val collectionAnnotation = ClassHelper.getAnnotationValue[Collection, String](classSymbol)

      // Extract any type members
      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap = tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap
      val typeMembers = tm collect {
        case m if !m.typeSignature.typeSymbol.isClass => // Ignore any user-set type declarations that aren't class parameters, e.g. type Foo = Int
          ClassHelper.TypeMember[T](m.name.decodedName.toString, m.typeSignature, classTypeParamMap(m.typeSignature.typeSymbol))
      }

      val hasEmptyConstructor = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      if (classSymbol.isJava && !hasEmptyConstructor)
        throw new IllegalStateException("ScalaJack does not support Java classes with a non-empty constructor.")

      //-------------------------------------------------------------------------------

      def inferConstructorValFields: List[ClassHelper.ClassFieldMember[T, Any]] = {
        constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
          case (member, index) =>
            val memberName = member.name.encodedName.toString

            val termName = tpe.member(TermName(memberName))
            val accessorMethodSymbol =
              if (termName.isMethod)
                termName.asMethod
              else
                throw new IllegalStateException("ScalaJack doesn't support non-val constructor fields (they can't be read by reflection)")
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

            // Exctract DBKey annotation if present
            val optionalDbKeyIndex = ClassHelper.getAnnotationValue[DBKey, Int](member, Some(0))

            // Extract MapName annotation if present
            val optionalMapName = ClassHelper.getAnnotationValue[MapName, String](member)

            val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
            ClassFieldMember[T, Any](
              index,
              optionalMapName.getOrElse(memberName),
              memberType,
              memberTypeAdapter,
              memberType /* FIXME */ ,
              accessorMethod,
              derivedValueClassConstructorMirror,
              ClassHelper.extractDefaultConstructorParamValueMethod(clazz, index + 1),
              memberClass,
              optionalDbKeyIndex,
              optionalMapName,
              None,
              None)
        })
      }

      def reflectScalaGetterSetterFields(startingIndex: Int): List[ClassFieldMember[T, Any]] = {
        var index = startingIndex
        val setters = tpe.members.filter(p => p.isPublic && p.isMethod && p.name.toString.endsWith("_$eq"))
        val getters = setters.map { s =>
          val simpleName = s.name.toString.stripSuffix("_$eq")
          tpe.members.find(f => f.name.toString == simpleName).getOrElse(throw new Exception("Boom--can't find getter for setter " + simpleName))
        }
        val getterSetter = getters.zip(setters)
        val setterWithIgnoreAnnotation = getterSetter.map {
          case (getter, setter) =>
            val foundPrivateVar = tpe.members.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == getter.name.toString.trim).headOption
            (setter, foundPrivateVar.map(ClassHelper.annotationExists[Ignore](_)).getOrElse(false) ||
              ClassHelper.annotationExists[Ignore](getter) ||
              ClassHelper.annotationExists[Ignore](setter))
        }
        setterWithIgnoreAnnotation.collect {
          case (s, ignore) if !ignore && s.owner != typeOf[SJCapture].typeSymbol =>
            index += 1
            bakeScalaPlainFieldMember(s, index - 1)
        }.toList
      }

      def bakeScalaPlainFieldMember(setterMethod: Symbol, index: Int): ClassFieldMember[T, Any] = {
        val simpleName = setterMethod.name.toString.stripSuffix("_$eq")

        // p is the actual member.  Find it from the given setterMethod by stripping the suffix.
        val p = tpe.members.find(f => f.name.toString == simpleName).getOrElse(throw new Exception("Boom--can't find getter for setter " + simpleName))

        val memberType = p.asMethod.returnType
        val declaredMemberType = tpe.typeSymbol.asType.toType.member(p.name).asMethod.returnType
        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        val (derivedValueClassConstructorMirror2, memberClass) =
          if (memberType.typeSymbol.isClass) {
            val memberClassSymbol = memberType.typeSymbol.asClass

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

        // Exctract DBKey and MapName annotations if present (Note: Here the annotation is not on the getter/setter but the private backing variable!)
        val foundPrivateVar = tpe.members.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
        val dbkeyAnno = foundPrivateVar.flatMap(ClassHelper.getAnnotationValue[DBKey, Int](_, Some(0)))

        val mapNameAnno = foundPrivateVar.flatMap(ClassHelper.getAnnotationValue[MapName, String](_))

        val isMaybe =
          foundPrivateVar.map(ClassHelper.annotationExists[Maybe](_)).getOrElse(false) ||
            ClassHelper.annotationExists[Maybe](p) ||
            ClassHelper.annotationExists[Maybe](setterMethod)

        ClassFieldMember[T, Any](
          index,
          mapNameAnno.getOrElse(simpleName),
          memberType,
          memberTypeAdapter,
          declaredMemberType,
          Reflection.methodToJava(p.asMethod),
          derivedValueClassConstructorMirror2,
          None, // defaultValueMirror not needed
          memberClass,
          dbkeyAnno,
          mapNameAnno,
          Some(setterMethod.asMethod),
          None,
          isMaybe
        )
      }

      def reflectJavaGetterSetterFields: List[ClassFieldMember[T, Any]] = {
        var index = 0

        // Figure out getters/setters, accouting for @Ignore
        Introspector.getBeanInfo(clazz).getPropertyDescriptors.toList.filterNot(_.getName == "class").collect {
          case DontIgnore_Java(propertyDescriptor) =>
            val memberType = tpe.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
            val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
            val declaredMemberType = tpe.typeSymbol.asType.toType.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
            index += 1

            // Extract Maybe annotation if present... For var: on private shadow member.  For getter/setter it could be on either...check both.
            val isMaybe =
              Option(propertyDescriptor.getReadMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == maybeClass)).isDefined ||
                Option(propertyDescriptor.getWriteMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == maybeClass)).isDefined

            ClassFieldMember[T, Any](
              index - 1,
              propertyDescriptor.getName,
              memberType,
              memberTypeAdapter,
              declaredMemberType,
              propertyDescriptor.getReadMethod,
              None,
              None, // defaultValueMirror not needed
              None,
              None,
              None,
              None,
              Some(propertyDescriptor.getWriteMethod),
              isMaybe
            )
        }
      }

      //-------------------------------------------------------------------------------

      val (constructorFields, allOtherFields) =
        if (classSymbol.isJava) {
          (List.empty[ClassHelper.ClassFieldMember[T, Any]], reflectJavaGetterSetterFields)
        } else {
          val constFields = inferConstructorValFields
          (constFields, reflectScalaGetterSetterFields(constFields.size))
        }

      if (classSymbol.isAbstract)
        throw new IllegalArgumentException("Unable to find a type adapter for " + classSymbol.name.toString + " (may be abstract or a dependency of an abstract class)")

      PlainClassTypeAdapter(
        classSymbol.name.toString(),
        typeMembers.map(t => (t.name, t)).toMap,
        ListMap(constructorFields.map(m => (m.name, m)): _*),
        ListMap(allOtherFields.map(m => (m.name, m)): _*),
        constructorMirror,
        isSJCapture,
        collectionAnnotation,
        !classSymbol.isJava
      )(context, tt)
    } else {
      next.typeAdapterOf[T]
    }
  }

}

object DontIgnore_Java {
  val ignoreClass = currentMirror.runtimeClass(typeOf[Ignore].typeSymbol.asClass)

  def unapply(javabeanPropertyDescriptor: PropertyDescriptor): Option[PropertyDescriptor] = {
    val isIgnore =
      Option(javabeanPropertyDescriptor.getReadMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == ignoreClass)).isDefined ||
        Option(javabeanPropertyDescriptor.getWriteMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == ignoreClass)).isDefined
    if (isIgnore)
      None
    else
      Some(javabeanPropertyDescriptor)
  }
}
