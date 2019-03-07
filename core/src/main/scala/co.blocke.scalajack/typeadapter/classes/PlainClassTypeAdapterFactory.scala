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
      val mapNameClass = currentMirror.runtimeClass(typeOf[MapName].typeSymbol.asClass)

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
              memberType,
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

        def ignoreSymbol(syms: List[Symbol], fieldName: String): Boolean =
          syms.foldRight(false) { case (f, acc) => acc || ClassHelper.annotationExists[Ignore](f) }

        val setters = tpe.members.filter(p => p.isPublic && p.isMethod && p.name.toString.endsWith("_$eq"))
        val getters = setters.map { s =>
          val simpleName = s.name.toString.stripSuffix("_$eq")
          tpe.members.find(f => f.name.toString == simpleName).getOrElse(throw new java.lang.IllegalStateException("Can't find corresponding getter for setter " + simpleName))
        }.toList

        // Find @Ignore (handling inheritance)
        val getterSetter = getters.zip(setters)
        val getterSetterWithIgnoreAnnotation = getterSetter.map {
          case (getter, setter) =>
            (getter, setter, tt.tpe.baseClasses.map { f =>
              val privateVar = f.typeSignature.members.find(z => z.isPrivate && !z.isMethod && z.name.toString.trim == getter.name.toString.trim)
              val privateIgnore = privateVar.map(ClassHelper.annotationExists[Ignore](_)).getOrElse(false)

              val inheritSetters = f.typeSignature.members.filter(_.name.toString == setter.name.toString).toList
              val inheritGetters = f.typeSignature.members.filter(_.name.toString == getter.name.toString).toList
              privateIgnore || ignoreSymbol(inheritSetters, getter.name.toString) || ignoreSymbol(inheritGetters, getter.name.toString)
            }.foldRight(false) { case (v, acc) => acc || v })
        }

        getterSetterWithIgnoreAnnotation.collect {
          case (g, s, ignore) if !ignore && s.owner != typeOf[SJCapture].typeSymbol =>
            index += 1
            bakeScalaPlainFieldMember(g, s, index - 1)
        }
      }

      def bakeScalaPlainFieldMember(getterMethod: Symbol, setterMethod: Symbol, index: Int): ClassFieldMember[T, Any] = {
        val simpleName = getterMethod.name.toString

        val memberType = getterMethod.asMethod.returnType
        val declaredMemberType = tpe.typeSymbol.asType.toType.member(getterMethod.name).asMethod.returnType
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

        // This finds field (and inherited) annotations
        var dbkeyAnno: Option[Int] = None
        var mapNameAnno: Option[String] = None
        var isMaybe: Boolean = false
        tt.tpe.baseClasses.foreach { f =>
          val privateVar = f.typeSignature.members.find(z => z.isPrivate && !z.isMethod && z.name.toString.trim == getterMethod.name.toString.trim)
          val db = privateVar.flatMap(ClassHelper.getAnnotationValue[DBKey, Int](_, Some(0)))
            .orElse(ClassHelper.getAnnotationValue[DBKey, Int](getterMethod, Some(0)))
            .orElse(ClassHelper.getAnnotationValue[DBKey, Int](setterMethod, Some(0)))
          val mapName = privateVar.flatMap(ClassHelper.getAnnotationValue[MapName, String](_))
            .orElse(ClassHelper.getAnnotationValue[MapName, String](getterMethod))
            .orElse(ClassHelper.getAnnotationValue[MapName, String](setterMethod))
          val maybe =
            privateVar.map(ClassHelper.annotationExists[Maybe](_)).getOrElse(false) ||
              ClassHelper.annotationExists[Maybe](getterMethod) ||
              ClassHelper.annotationExists[Maybe](setterMethod)
          if (db.isDefined)
            dbkeyAnno = db
          if (mapName.isDefined)
            mapNameAnno = mapName
          if (maybe)
            isMaybe = true
        }

        ClassFieldMember[T, Any](
          index,
          mapNameAnno.getOrElse(simpleName),
          memberType,
          memberTypeAdapter,
          declaredMemberType,
          Reflection.methodToJava(getterMethod.asMethod),
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

            val mapNameAnno =
              Option(propertyDescriptor.getReadMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == mapNameClass)).map(_.asInstanceOf[MapName].name) orElse
                Option(propertyDescriptor.getWriteMethod).flatMap(_.getDeclaredAnnotations.find(_.annotationType() == mapNameClass)).map(_.asInstanceOf[MapName].name)

            ClassFieldMember[T, Any](
              index - 1,
              mapNameAnno.getOrElse(propertyDescriptor.getName),
              memberType,
              memberTypeAdapter,
              declaredMemberType,
              propertyDescriptor.getReadMethod,
              None,
              None, // defaultValueMirror not needed
              None,
              None,
              mapNameAnno, // mapName
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
      // $COVERAGE-OFF$Can't really test this.  It is not supposed to ever happen.  What can you parse that isn't some kind of class?
      next.typeAdapterOf[T]
      // $COVERAGE-ON$
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
