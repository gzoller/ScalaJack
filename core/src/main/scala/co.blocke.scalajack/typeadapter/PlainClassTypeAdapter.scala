package co.blocke.scalajack
package typeadapter

import java.beans.Introspector
import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.CaseClassTypeAdapter.FieldMember
import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

import scala.collection.mutable
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe
import scala.util.{ Failure, Success, Try }

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class PlainFieldMember[Owner, T](
      index:             Int,
      name:              String,
      valueType:         Type,
      valueTypeAdapter:  TypeAdapter[T],
      declaredValueType: Type,
      valueGetterMethod: Method,
      // Java & Scala need different setters.  Scala needs to properly set ValueClass values,
      // which can't be done using a Java method call.  Of course Java can *only* use a Java
      // method call, so... we have both.
      valueSetterMethodSymbol:            Option[MethodSymbol], // for Scala
      valueSetterMethod:                  Option[Method], // for Java
      derivedValueClassConstructorMirror: Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      fieldMapName:                       Option[String]
  ) extends ClassFieldMember[Owner, T] {

    override type Value = T

    val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    override val valueTypeTag = new TypeTag[T] {

      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[T] = ???

      override val mirror: universe.Mirror = currentMirror

      override def tpe: universe.Type = valueType

    }

    def valueIn(instance: Owner): T = {
      val value = valueGetterMethod.invoke(instance)

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

    def valueSet(instance: Any, value: Object): Unit =
      valueSetterMethodSymbol match {
        case Some(vsms) => currentMirror.reflect(instance).reflectMethod(vsms)(value)
        case None       => valueSetterMethod.get.invoke(instance, value.asInstanceOf[Object])
      }

    override def readValue(reader: Reader): Value = {
      valueTypeAdapter.read(reader)
    }

    override def writeValue(value: T, writer: Writer): Unit = {
      valueTypeAdapter.write(value, writer)
    }

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    override def defaultValue: Option[T] = if (isOptional) { Some(None).asInstanceOf[Option[T]] } else None

    override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] = None

    override def isStringValue: Boolean =
      valueTypeAdapter.isInstanceOf[StringKind]

  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, typeTag: TypeTag[T]): TypeAdapter[T] = {
    val tpe = typeTag.tpe
    if (classSymbol.isClass) {

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]
      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      def inferConstructorValFields: List[ClassFieldMember[T, Any]] =
        Try {
          // Might fail if *all* the constructor's parameters aren't val notated
          constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
            case (member, index) =>
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

              val memberType = member.asTerm.typeSignature

              // Exctract DBKey annotation if present
              val dbkeyAnnotation = member.annotations.find(_.tree.tpe =:= typeOf[DBKey])
                .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                  .value().value).asInstanceOf[Option[Int]]

              // Exctract MapName annotation if present
              val mapNameAnnotation = member.annotations.find(_.tree.tpe =:= typeOf[MapName])
                .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                  .value().value).asInstanceOf[Option[String]]

              val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
              FieldMember[T, Any](index, memberName, memberType, memberTypeAdapter, memberType /* FIXME */ , accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, None, memberClass, dbkeyAnnotation, mapNameAnnotation, member.annotations)
          })
        } match {
          case Success(m) => m
          case Failure(x) =>
            List.empty[ClassFieldMember[T, Any]]
        }

      def dontIgnore(p: Symbol) = {
        // Annoying... @Ignore may be on backing field in a superclass...so we must go find it.
        val includeSuper = tpe.members ++ tpe.typeSymbol.asClass.baseClasses.map(c => c.typeSignature.members).flatten
        var foundPrivateVar = includeSuper.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
        val ignoreAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[Ignore]))
        ignoreAnno.isEmpty
      }

      def reflectScalaGetterSetterFields: List[ClassFieldMember[T, Any]] = {
        tpe.members.filter(p => p.isPublic && p.isMethod).collect {
          // Scala case
          case p if (dontIgnore(p) && tpe.member(TermName(p.name.toString + "_$eq")) != NoSymbol && p.owner != typeOf[SJCapture].typeSymbol) =>
            val memberType = p.asMethod.returnType
            val declaredMemberType = tpe.typeSymbol.asType.toType.member(p.name).asMethod.returnType
            val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]

            val (derivedValueClassConstructorMirror, memberClass) =
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
            var foundPrivateVar = tpe.members.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
            val dbkeyAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[DBKey])
              .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                .value().value).asInstanceOf[Option[Int]])
            val mapNameAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[MapName])
              .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                .value().value).asInstanceOf[Option[String]])

            PlainFieldMember[T, Any](
              0,
              p.name.encodedName.toString,
              memberType,
              memberTypeAdapter,
              declaredMemberType,
              Reflection.methodToJava(p.asMethod),
              Some(tpe.member(TermName(p.name.toString + "_$eq")).asMethod),
              None,
              derivedValueClassConstructorMirror,
              memberClass,
              dbkeyAnno,
              mapNameAnno
            )
        }.toList.zipWithIndex.map { case (pm, index) => pm.copy(index = index).asInstanceOf[PlainFieldMember[T, Any]] }
      }

      def reflectJavaGetterSetterFields: List[ClassFieldMember[T, Any]] = {
        val clazz = currentMirror.runtimeClass(tpe.typeSymbol.asClass)
        Introspector.getBeanInfo(clazz).getPropertyDescriptors.toList.filterNot(_.getName == "class").map { propertyDescriptor =>
          val memberType = tpe.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
          val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
          val declaredMemberType = tpe.typeSymbol.asType.toType.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
          PlainFieldMember[Any, Any](
            0,
            propertyDescriptor.getName,
            memberType,
            memberTypeAdapter,
            declaredMemberType,
            propertyDescriptor.getReadMethod,
            None,
            Some(propertyDescriptor.getWriteMethod),
            None,
            None,
            None,
            None
          )
        }.zipWithIndex.map { case (pm, index) => pm.asInstanceOf[PlainFieldMember[T, Any]].copy(index = index).asInstanceOf[PlainFieldMember[T, Any]] }
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = classSymbol.annotations.find(_.tree.tpe =:= typeOf[Collection])
        .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
          .value().value).asInstanceOf[Option[String]]

      def dbKeys[Owner](members: List[ClassFieldMember[Owner, Any]]): List[ClassFieldMember[Owner, Any]] = members.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

      val hasEmptyConstructor = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      inferConstructorValFields match {
        case members if (!members.isEmpty) =>
          // Because all the val fields were found in the constructor we can use a normal CaseClassTypeAdapter
          CaseClassTypeAdapter[T](context, tpe, constructorMirror, memberNameTypeAdapter, context.typeAdapterOf[Type], Nil, members, isSJCapture, collectionAnnotation)
        case _ if (!classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectScalaGetterSetterFields
          PlainClassTypeAdapter[T](tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture, dbKeys(members), collectionAnnotation).asInstanceOf[TypeAdapter[T]]
        case _ if (classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectJavaGetterSetterFields
          PlainClassTypeAdapter[T](tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture, dbKeys(members), collectionAnnotation).asInstanceOf[TypeAdapter[T]]
        case x =>
          next.typeAdapterOf[T]
      }

    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class PlainClassTypeAdapter[T](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[ClassFieldMember[T, _]],
    isSJCapture:           Boolean,
    dbKeys:                List[ClassFieldMember[T, _]],
    collectionName:        Option[String]               = None
) extends TypeAdapter[T] {

  val membersByName = members.map(member => member.name -> member.asInstanceOf[ClassFieldMember[T, Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.BeginObject =>
        val numberOfMembers = members.length

        val found = new mutable.BitSet(numberOfMembers)
        var foundCount = 0

        reader.beginObject()

        val asBuilt = constructorMirror.apply().asInstanceOf[T] // call 0-parameter constructor

        var savedPos = reader.position
        while (reader.hasMoreMembers) {
          membersByName.get(memberNameTypeAdapter.read(reader)) match {
            case Some(member) =>
              member.asInstanceOf[PlainFieldMember[T, _]].valueSet(asBuilt, member.readValue(reader).asInstanceOf[Object])
              found(member.index) = true

            case None =>
              reader.skipValue()
          }
        }

        reader.endObject()

        if (foundCount != numberOfMembers)
          for (member <- members if !found(member.index)) {
            member.defaultValue.getOrElse(
              throw new IllegalStateException(s"Required field ${member.name} in class ${tpe.typeSymbol.fullName} is missing from input and has no specified default value\n" + reader.showError())
            )
          }

        if (isSJCapture) {
          reader.position = savedPos
          val captured = scala.collection.mutable.Map.empty[String, Any]
          while (reader.hasMoreMembers) {
            val memberName = memberNameTypeAdapter.read(reader)
            membersByName.get(memberName) match {
              case Some(member) => reader.skipValue // do nothing... already built class
              case None =>
                captured.put(memberName, reader.captureValue())
            }
          }
          asBuilt.asInstanceOf[SJCapture].captured = captured
        }

        asBuilt

      case TokenType.Null =>
        reader.readNull().asInstanceOf[T]
    }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member <- members) {
        val memberValue = member.valueIn(value)

        memberNameTypeAdapter.write(member.name, writer)
        member.writeValue(memberValue, writer)
      }
      value match {
        case sjc: SJCapture =>
          sjc.captured.foreach {
            case (memberName, valueString) =>
              memberNameTypeAdapter.write(memberName, writer)
              writer.writeRawValue(valueString.asInstanceOf[String])
          }
        case _ =>
      }

      writer.endObject()
    }
}
