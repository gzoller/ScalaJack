package co.blocke.scalajack
package typeadapter

import java.beans.Introspector
import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.CaseClassTypeAdapter.FieldMember

import scala.collection.immutable.List
import scala.language.existentials
import scala.reflect.ClassTag
import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe
import scala.util.{ Failure, Success, Try }

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  trait PlainFieldMember[Owner] extends ClassFieldMember[Owner] {
    implicit def ownerClassTag: ClassTag[Owner]
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val valueGetterMethod: Method
    // Java & Scala need different setters.  Scala needs to properly set ValueClass values,
    // which can't be done using a Java method call.  Of course Java can *only* use a Java
    // method call, so... we have both.
    val valueSetterMethodSymbol: Option[MethodSymbol] // for Scala
    val valueSetterMethod: Option[Method] // for Java
    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]

    def deserializer: Deserializer[Value] = valueTypeAdapter.deserializer

    def serializer: Serializer[Value] = valueTypeAdapter.serializer

    lazy val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    override lazy val valueTypeTag = new TypeTag[Value] {

      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[Value] = ???

      override val mirror: universe.Mirror = currentMirror

      override def tpe: universe.Type = valueType

    }

    def valueIn(tagged: TypeTagged[Owner]): TypeTagged[Value] =
      tagged match {
        case TypeTagged(instance) =>
          val value = valueGetterMethod.invoke(instance)

          if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
            TypeTagged.inferFromRuntimeClass[Value](value.asInstanceOf[Value])
          } else {
            derivedValueClassConstructorMirror match {
              case Some(methodMirror) =>
                TypeTagged.inferFromRuntimeClass[Value](methodMirror.apply(value).asInstanceOf[Value])

              case None =>
                TypeTagged.inferFromRuntimeClass[Value](value.asInstanceOf[Value])
            }
          }
      }

    def valueSet(instance: Owner, value: Value): Unit =
      valueSetterMethodSymbol match {
        case Some(vsms) => currentMirror.reflect(instance).reflectMethod(vsms)(value)
        case None       => valueSetterMethod.get.invoke(instance, value.asInstanceOf[Object])
      }

    override def deserializeValueFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserializeFromNothing(path)

    override def deserializeValue[J](path: Path, json: J)(implicit ops: JsonOps[J], guidance: SerializationGuidance): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserialize(path, json)

    override def serializeValue[J](tagged: TypeTagged[Value])(implicit ops: JsonOps[J], guidance: SerializationGuidance): SerializationResult[J] =
      valueTypeAdapter.serializer.serialize(tagged)

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    override def defaultValue: Option[Value] = if (isOptional) {
      Some(None).asInstanceOf[Option[Value]]
    } else None

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

      def newInstance(): T = constructorMirror.apply().asInstanceOf[T]

      def inferConstructorValFields: List[ClassFieldMember[T]] =
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
              val mapNameAnnotation: Option[String] = member.annotations.find(_.tree.tpe =:= typeOf[MapName])
                .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                  .value().value).asInstanceOf[Option[String]]

              val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
              FieldMember[T, Any](index, mapNameAnnotation.getOrElse(memberName), memberType, memberTypeAdapter, memberType /* FIXME */ , accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, None, memberClass, dbkeyAnnotation, mapNameAnnotation, member.annotations)
          })
        } match {
          case Success(m) => m
          case Failure(x) =>
            List.empty[ClassFieldMember[T]]
        }

      def dontIgnore(p: Symbol) = {
        // Annoying... @Ignore may be on backing field in a superclass...so we must go find it.
        val includeSuper = tpe.members ++ tpe.typeSymbol.asClass.baseClasses.map(c => c.typeSignature.members).flatten
        val foundPrivateVar = includeSuper.filter(z => z.isPrivate && !z.isMethod && z.name.toString.trim == p.name.toString.trim).headOption
        val ignoreAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[Ignore]))
        ignoreAnno.isEmpty
      }

      def reflectScalaGetterSetterFields: List[PlainFieldMember[T]] = {
        var i = 0

        tpe.members.filter(p => p.isPublic && p.isMethod).collect {
          // Scala case
          case p if (dontIgnore(p) && tpe.member(TermName(p.name.toString + "_$eq")) != NoSymbol && p.owner != typeOf[SJCapture].typeSymbol) =>
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
            val dbkeyAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[DBKey])
              .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                .value().value).asInstanceOf[Option[Int]])
            val mapNameAnno = foundPrivateVar.flatMap(_.annotations.find(_.tree.tpe =:= typeOf[MapName])
              .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
                .value().value).asInstanceOf[Option[String]])

            new PlainFieldMember[T] {
              override type Value = Any
              override implicit val ownerClassTag: ClassTag[T] = ClassTag(runtimeClassOf[T])
              override val index: Int = {
                val idx = i
                i += 1
                idx
              }
              override val name: String = mapNameAnno.getOrElse(p.name.encodedName.toString)
              override val valueType: Type = memberType
              override val valueTypeAdapter: TypeAdapter[Value] = memberTypeAdapter
              override val declaredValueType: Type = declaredMemberType
              override val valueGetterMethod: Method = Reflection.methodToJava(p.asMethod)
              override val valueSetterMethodSymbol: Option[MethodSymbol] = Some(tpe.member(TermName(p.name.toString + "_$eq")).asMethod)
              override val valueSetterMethod: Option[Method] = None
              override val derivedValueClassConstructorMirror: Option[MethodMirror] = derivedValueClassConstructorMirror2
              override val outerClass: Option[java.lang.Class[_]] = memberClass
              override val dbKeyIndex: Option[Int] = dbkeyAnno
            }
        }.toList
      }

      def reflectJavaGetterSetterFields: List[PlainFieldMember[T]] = {
        var i = 0

        val clazz = currentMirror.runtimeClass(tpe.typeSymbol.asClass)
        Introspector.getBeanInfo(clazz).getPropertyDescriptors.toList.filterNot(_.getName == "class").map { propertyDescriptor =>
          val memberType = tpe.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
          val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
          val declaredMemberType = tpe.typeSymbol.asType.toType.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
          new PlainFieldMember[T] {
            override implicit val ownerClassTag: ClassTag[T] = ClassTag(runtimeClassOf[T])
            override type Value = Any
            override val index: Int = {
              val idx = i
              i += 1
              idx
            }
            override val name: String = propertyDescriptor.getName
            override val valueType: Type = memberType
            override val valueTypeAdapter: TypeAdapter[Value] = memberTypeAdapter
            override val declaredValueType: Type = declaredMemberType
            override val valueGetterMethod: Method = propertyDescriptor.getReadMethod
            override val valueSetterMethodSymbol: Option[MethodSymbol] = None
            override val valueSetterMethod: Option[Method] = Some(propertyDescriptor.getWriteMethod)
            override val derivedValueClassConstructorMirror: Option[MethodMirror] = None
            override val outerClass: Option[java.lang.Class[_]] = None
            override val dbKeyIndex: Option[Int] = None
          }
        }
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = classSymbol.annotations.find(_.tree.tpe =:= typeOf[Collection])
        .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
          .value().value).asInstanceOf[Option[String]]

      val hasEmptyConstructor = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      inferConstructorValFields match {
        case members if (!members.isEmpty) =>
          // Because all the val fields were found in the constructor we can use a normal CaseClassTypeAdapter
          CaseClassTypeAdapter[T](
            new ClassDeserializerUsingReflectedConstructor[T](context, constructorMirror, context.typeAdapterOf[Type].deserializer, Nil, members, isSJCapture),
            new ClassSerializer[T](context, constructorMirror, context.typeAdapterOf[Type].serializer, Nil, members, isSJCapture),
            context, tpe, constructorMirror, memberNameTypeAdapter, context.typeAdapterOf[Type], Nil, members, isSJCapture, collectionAnnotation)
        case _ if (!classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectScalaGetterSetterFields
          PlainClassTypeAdapter[T](
            new PlainClassDeserializer[T](members, (() => newInstance()), isSJCapture), // FIXME
            new PlainClassSerializer[T](members, isSJCapture))
        case _ if (classSymbol.isJava && hasEmptyConstructor) =>
          val members = reflectJavaGetterSetterFields
          PlainClassTypeAdapter[T](
            new PlainClassDeserializer[T](members, (() => newInstance()), isSJCapture), // FIXME
            new PlainClassSerializer[T](members, isSJCapture))
        // There's no support for Java classes with non-empty constructors.  If multiple, which one to use?
        // Opens the door for uncertain results.
        case x =>
          next.typeAdapterOf[T]
      }

    } else {
      next.typeAdapterOf[T]
    }
  }

}

case class PlainClassTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T]) extends TypeAdapter[T]

/*
case class PlainClassTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    caseClassType:             Type,
    constructorMirror:         MethodMirror,
    tpe:                       Type,
    memberNameTypeAdapter:     TypeAdapter[MemberName],
    members:                   List[PlainFieldMember[T]],
    isSJCapture:               Boolean,
    dbKeys:                    List[PlainFieldMember[T]],
    collectionName:            Option[String]            = None) extends TypeAdapter[T]
*/ 