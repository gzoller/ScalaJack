package co.blocke.scalajack
package typeadapter

import model.{ ClassHelper, _ }
import util.Reflection
import java.beans.{ Introspector, PropertyDescriptor }

import ClassHelper._
import co.blocke.scalajack.SJCapture

import scala.collection.immutable.{ List, ListMap }
import scala.collection.mutable
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](
      classSymbol: ClassSymbol,
      next:        TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] = {
    val tpe = tt.tpe
    if (classSymbol.isAbstract)
      throw new IllegalArgumentException(
        "Unable to find a type adapter for " + classSymbol.name.toString + " (abstract class or a dependency of an abstract class)"
      )

    if (classSymbol.isClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val clazz = currentMirror.runtimeClass(classSymbol)

      // For Java classes
      val maybeClass =
        currentMirror.runtimeClass(typeOf[Optional].typeSymbol.asClass)
      val nameChangeClass =
        currentMirror.runtimeClass(typeOf[Change].typeSymbol.asClass)

      // Exctract Collection name annotation if present
      val collectionAnnotation =
        ClassHelper.getAnnotationValue[Collection, String](classSymbol)

      // Extract any type members
      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap =
        tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap
      val typeMembers = tm.map(
        m =>
          ClassHelper.TypeMember[T](
            m.name.decodedName.toString,
            m.typeSignature,
            classTypeParamMap(m.typeSignature.typeSymbol)
          )
      )

      val hasEmptyConstructor =
        constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      if (classSymbol.isJava && !hasEmptyConstructor)
        throw new IllegalStateException(
          "ScalaJack does not support Java classes with a non-empty constructor."
        )

      //-------------------------------------------------------------------------------

      def inferConstructorValFields: List[ClassHelper.ClassFieldMember[T, Any]] = {
        constructorSymbol
          .typeSignatureIn(tpe)
          .paramLists
          .flatten
          .zipWithIndex
          .map({
            case (member, index) =>
              val memberName = member.name.encodedName.toString

              val termName = tpe.member(TermName(memberName))
              val accessorMethodSymbol =
                if (termName.isMethod)
                  termName.asMethod
                else
                  throw new IllegalStateException(
                    "ScalaJack doesn't support non-val constructor fields (they can't be read by reflection)"
                  )
              val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

              val (derivedValueClassConstructorMirror, memberClass) =
                if (member.typeSignature.typeSymbol.isClass) {
                  val memberClassSymbol =
                    member.typeSignature.typeSymbol.asClass

                  if (memberClassSymbol.isDerivedValueClass) {
                    val memberClass =
                      currentMirror.runtimeClass(memberClassSymbol)
                    // The accessor will actually return the "inner" value, not the value class.
                    val constructorMethodSymbol =
                      memberClassSymbol.primaryConstructor.asMethod
                    //              val innerClass = currentMirror.runtimeClass(constructorMethodSymbol.paramLists.flatten.head.info.typeSymbol.asClass)
                    (
                      Some(
                        currentMirror
                          .reflectClass(memberClassSymbol)
                          .reflectConstructor(constructorMethodSymbol)
                      ),
                        Some(memberClass)
                    )
                  } else {
                    (None, None)
                  }
                } else {
                  (None, None)
                }

              val memberType = member.asTerm.typeSignature

              // Exctract DBKey annotation if present
              val optionalDbKeyIndex =
                ClassHelper.getAnnotationValue[DBKey, Int](member, Some(0))

              // Extract MapName annotation if present
              val optionalMapName =
                ClassHelper.getAnnotationValue[Change, String](member)

              val memberTypeAdapter =
                taCache.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
              ClassFieldMember[T, Any](
                index,
                optionalMapName.getOrElse(memberName),
                memberType,
                memberTypeAdapter,
                memberType,
                accessorMethod,
                derivedValueClassConstructorMirror,
                ClassHelper
                  .extractDefaultConstructorParamValueMethod(clazz, index + 1),
                memberClass,
                optionalDbKeyIndex,
                optionalMapName,
                tt.tpe,
                None,
                None
              )
          })
      }

      def reflectScalaGetterSetterFields(
          startingIndex: Int
      ): List[ClassFieldMember[T, Any]] = {
        var index = startingIndex

        def ignoreSymbol(syms: List[Symbol], fieldName: String): Boolean =
          syms.foldRight(false) {
            case (f, acc) => acc || ClassHelper.annotationExists[Ignore](f)
          }

        val setters = tpe.members.filter(
          p => p.isPublic && p.isMethod && p.name.toString.endsWith("_$eq")
        )
        val getters = setters.map { s =>
          val simpleName = s.name.toString.stripSuffix("_$eq")
          tpe.members
            .find(f => f.name.toString == simpleName)
            .getOrElse(
              throw new java.lang.IllegalStateException(
                "Can't find corresponding getter for setter " + simpleName
              )
            )
        }.toList

        // Find @Ignore (handling inheritance)
        val getterSetter = getters.zip(setters)
        val getterSetterWithIgnoreAnnotation = getterSetter.map {
          case (getter, setter) =>
            (
              getter,
              setter,
              tt.tpe.baseClasses
              .map { f =>
                val privateVar = f.typeSignature.members.find(
                  z =>
                    z.isPrivate && !z.isMethod && z.name.toString.trim == getter.name.toString.trim
                )
                val privateIgnore =
                  privateVar.exists(ClassHelper.annotationExists[Ignore](_))

                val inheritSetters = f.typeSignature.members
                  .filter(_.name.toString == setter.name.toString)
                  .toList
                val inheritGetters = f.typeSignature.members
                  .filter(_.name.toString == getter.name.toString)
                  .toList
                privateIgnore || ignoreSymbol(
                  inheritSetters,
                  getter.name.toString
                ) || ignoreSymbol(inheritGetters, getter.name.toString)
              }
              .foldRight(false) { case (v, acc) => acc || v }
            )
        }

        getterSetterWithIgnoreAnnotation.collect {
          case (g, s, ignore) if !ignore && s.owner != typeOf[SJCapture].typeSymbol =>
            index += 1
            bakeScalaPlainFieldMember(g, s, index - 1)
        }
      }

      def bakeScalaPlainFieldMember(
          getterMethod: Symbol,
          setterMethod: Symbol,
          index:        Int): ClassFieldMember[T, Any] = {
        val simpleName = getterMethod.name.toString

        val memberType = getterMethod.asMethod.returnType
        val declaredMemberType = tpe.typeSymbol.asType.toType
          .member(getterMethod.name)
          .asMethod
          .returnType
        val memberTypeAdapter =
          taCache.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

        val (derivedValueClassConstructorMirror2, memberClass) =
          if (memberType.typeSymbol.isClass) {
            val memberClassSymbol = memberType.typeSymbol.asClass

            if (memberClassSymbol.isDerivedValueClass) {
              val memberClass = currentMirror.runtimeClass(memberClassSymbol)
              // The accessor will actually return the "inner" value, not the value class.
              val constructorMethodSymbol =
                memberClassSymbol.primaryConstructor.asMethod
              //              val innerClass = currentMirror.runtimeClass(constructorMethodSymbol.paramLists.flatten.head.info.typeSymbol.asClass)
              (
                Some(
                  currentMirror
                    .reflectClass(memberClassSymbol)
                    .reflectConstructor(constructorMethodSymbol)
                ),
                  Some(memberClass)
              )
            } else {
              (None, None)
            }
          } else {
            (None, None)
          }

        // This finds field (and inherited) annotations
        var dbkeyAnno: Option[Int] = None
        var nameChangeAnno: Option[String] = None
        var hasOptionalAnnotation: Boolean = false
        tt.tpe.baseClasses.foreach { f =>
          val privateVar = f.typeSignature.members.find(
            z =>
              z.isPrivate && !z.isMethod && z.name.toString.trim == getterMethod.name.toString.trim
          )
          val db = privateVar
            .flatMap(ClassHelper.getAnnotationValue[DBKey, Int](_, Some(0)))
            .orElse(
              ClassHelper.getAnnotationValue[DBKey, Int](getterMethod, Some(0))
            )
            .orElse(
              ClassHelper.getAnnotationValue[DBKey, Int](setterMethod, Some(0))
            )
          val nameChange = privateVar
            .flatMap(ClassHelper.getAnnotationValue[Change, String](_))
            .orElse(
              ClassHelper.getAnnotationValue[Change, String](getterMethod)
            )
            .orElse(
              ClassHelper.getAnnotationValue[Change, String](setterMethod)
            )
          val maybe =
            privateVar.exists(ClassHelper.annotationExists[Optional](_)) ||
              ClassHelper.annotationExists[Optional](getterMethod) ||
              ClassHelper.annotationExists[Optional](setterMethod)
          if (db.isDefined)
            dbkeyAnno = db
          if (nameChange.isDefined)
            nameChangeAnno = nameChange
          if (maybe)
            hasOptionalAnnotation = true
        }

        ClassFieldMember[T, Any](
          index,
          nameChangeAnno.getOrElse(simpleName),
          memberType,
          memberTypeAdapter,
          declaredMemberType,
          Reflection.methodToJava(getterMethod.asMethod),
          derivedValueClassConstructorMirror2,
          None, // defaultValueMirror not needed
          memberClass,
          dbkeyAnno,
          nameChangeAnno,
          tt.tpe,
          Some(setterMethod.asMethod),
          None,
          hasOptionalAnnotation
        )
      }

      def reflectJavaGetterSetterFields: List[ClassFieldMember[T, Any]] = {
        var index = 0

        // Figure out getters/setters, accouting for @Ignore
        Introspector
          .getBeanInfo(clazz)
          .getPropertyDescriptors
          .toList
          .filterNot(_.getName == "class")
          .collect {
            case DontIgnore_Java(propertyDescriptor) =>
              val memberType = tpe
                .member(TermName(propertyDescriptor.getReadMethod.getName))
                .asMethod
                .returnType
              val memberTypeAdapter =
                taCache.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
              val declaredMemberType = tpe.typeSymbol.asType.toType
                .member(TermName(propertyDescriptor.getReadMethod.getName))
                .asMethod
                .returnType
              index += 1

              // Extract Optional annotation if present... For var: on private shadow member.  For getter/setter it could be on either...check both.
              val hasOptionalAnnotation =
                Option(propertyDescriptor.getReadMethod)
                  .flatMap(
                    _.getDeclaredAnnotations
                      .find(_.annotationType() == maybeClass)
                  )
                  .isDefined ||
                  Option(propertyDescriptor.getWriteMethod)
                  .flatMap(
                    _.getDeclaredAnnotations
                      .find(_.annotationType() == maybeClass)
                  )
                  .isDefined

              val nameChangeAnno =
                Option(propertyDescriptor.getReadMethod)
                  .flatMap(
                    _.getDeclaredAnnotations
                      .find(_.annotationType() == nameChangeClass)
                  )
                  .map(_.asInstanceOf[Change].name) orElse
                  Option(propertyDescriptor.getWriteMethod)
                  .flatMap(
                    _.getDeclaredAnnotations
                      .find(_.annotationType() == nameChangeClass)
                  )
                  .map(_.asInstanceOf[Change].name)

              ClassFieldMember[T, Any](
                index - 1,
                nameChangeAnno.getOrElse(propertyDescriptor.getName),
                memberType,
                memberTypeAdapter,
                declaredMemberType,
                propertyDescriptor.getReadMethod,
                None,
                None, // defaultValueMirror not needed
                None,
                None,
                nameChangeAnno, // nameChange
                tt.tpe,
                None,
                Some(propertyDescriptor.getWriteMethod),
                hasOptionalAnnotation
              )
          }
      }

      //-------------------------------------------------------------------------------

      if (classSymbol.isJava) {
        val setterFields = reflectJavaGetterSetterFields
        val fieldBits = {
          val bits = mutable.BitSet()
          setterFields.indices map (n => bits += n)
          bits
        }
        PlainClassTypeAdapter(
          classSymbol.name.toString,
          typeMembers.map(t => (t.name, t)).toMap,
          Map(setterFields.map(m => (m.name, m)): _*),
          setterFields.map(_.name).toList,
          0,
          new Array[Any](setterFields.size), // args template
          fieldBits,
          constructorMirror,
          isSJCapture,
          collectionAnnotation,
          !classSymbol.isJava
        )(taCache, tt)

      } else {
        // 1. Get Constructor fields
        val constructorFields = inferConstructorValFields
        // 2. Get non-constructor fields (either Scala bean/setters, or public var w/o @Ignore)
        val nonConstructorFields = reflectScalaGetterSetterFields(
          constructorFields.size
        )
        val argsTemplate =
          new Array[Any](constructorFields.size + nonConstructorFields.size)
        val allFields = constructorFields ++ nonConstructorFields
        val fieldBits = {
          val bits = mutable.BitSet()
          allFields.indices map (n => bits += n)
          bits
        }
        // Set bits for fields w/provided default values
        constructorFields.map(
          f =>
            if (f.defaultValue.isDefined) {
              argsTemplate(f.index) = f.defaultValue.get
              fieldBits -= f.index
            }
        )
        // same for setters
        //        nonConstructorFields.map(
        //          f =>
        //            if (f.isOptional)
        //              fieldBits -= f.index)

        PlainClassTypeAdapter(
          classSymbol.name.toString,
          typeMembers.map(t => (t.name, t)).toMap,
          Map(allFields.map(m => (m.name, m)): _*),
          nonConstructorFields.map(_.name),
          constructorFields.size,
          argsTemplate,
          fieldBits,
          constructorMirror,
          isSJCapture,
          collectionAnnotation,
          !classSymbol.isJava
        )(taCache, tt)
      }
    } else {
      // $COVERAGE-OFF$Can't really test this.  It is not supposed to ever happen.  What can you parse that isn't some kind of class?
      next.typeAdapterOf[T]
      // $COVERAGE-ON$
    }
  }

}

object DontIgnore_Java {
  val ignoreClass: Class[_] =
    currentMirror.runtimeClass(typeOf[Ignore].typeSymbol.asClass)

  def unapply(
      javabeanPropertyDescriptor: PropertyDescriptor
  ): Option[PropertyDescriptor] = {
    val isIgnore =
      Option(javabeanPropertyDescriptor.getReadMethod)
        .flatMap(
          _.getDeclaredAnnotations.find(_.annotationType() == ignoreClass)
        )
        .isDefined ||
        Option(javabeanPropertyDescriptor.getWriteMethod)
        .flatMap(
          _.getDeclaredAnnotations.find(_.annotationType() == ignoreClass)
        )
        .isDefined
    if (isIgnore)
      None
    else
      Some(javabeanPropertyDescriptor)
  }
}
