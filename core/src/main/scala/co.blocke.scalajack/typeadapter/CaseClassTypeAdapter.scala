package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import scala.annotation.Annotation
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, NoType, TermName, Type, TypeTag, typeOf, Annotation => UAnnotation }

trait ClassMember[Owner, T] extends ClassLikeTypeAdapter.Member[Owner] {
  def dbKeyIndex: Option[Int]
  def valueTypeAdapter: TypeAdapter[T]
}

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[Owner, T](
      index:                              Int,
      name:                               String,
      valueTypeAdapter:                   TypeAdapter[T],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      annotations:                        List[UAnnotation]
  ) extends ClassMember[Owner, T] {

    val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    override type Value = T

    override def valueIn(instance: Owner): Value = {
      val value = valueAccessorMethod.invoke(instance)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[Value]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) =>
            methodMirror.apply(value).asInstanceOf[Value]

          case None =>
            value.asInstanceOf[Value]
        }
      }
    }

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    override def defaultValue: Option[Value] =
      defaultValueMirror.map(_.apply().asInstanceOf[T]).orElse(if (isOptional) { Some(None).asInstanceOf[Option[T]] } else None)

    override def readValue(reader: Reader): Value =
      valueTypeAdapter.read(reader)

    override def writeValue(value: Value, writer: Writer): Unit =
      valueTypeAdapter.write(value, writer)

    override def annotation[A <: Annotation](implicit tt: TypeTag[A]): Option[A] =
      //      annotations.find(_.tree.productElement(0))
      ???

  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      val isSJCapture = !(tt.tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

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

          val optionalDbKeyIndex = member.annotations.find(_.isInstanceOf[DBKey]).map(_.asInstanceOf[DBKey]).map(_.index)

          val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
          Member[T, Any](index, memberName, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass, optionalDbKeyIndex, member.annotations)
      })

      // Exctract Collection name annotation if present
      val collectionAnnotation = classSymbol.annotations.find(_.tree.tpe =:= typeOf[Collection])
        .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
          .value().value).asInstanceOf[Option[String]]

      val dbKeys = members.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

      CaseClassTypeAdapter[T](tt.tpe, constructorMirror, tt.tpe, memberNameTypeAdapter, members, members.length, isSJCapture, dbKeys, collectionAnnotation)
    } else {
      next.typeAdapterOf[T](context)
    }

}

case class CaseClassTypeAdapter[T](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[ClassMember[T, _]],
    numberOfMembers:       Int,
    isSJCapture:           Boolean,
    dbKeys:                List[ClassMember[T, _]],
    collectionName:        Option[String]          = None
) extends ClassLikeTypeAdapter[T] {

  val membersByName = members.map(member => member.name → member.asInstanceOf[ClassMember[T, Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.BeginObject =>
        val arguments = new Array[Any](numberOfMembers)
        val found = new Array[Boolean](numberOfMembers)
        var foundCount = 0

        reader.beginObject()

        var savedPos = reader.position
        while (reader.hasMoreMembers) {
          membersByName.get(memberNameTypeAdapter.read(reader)) match {
            case Some(member) =>
              arguments(member.index) = member.readValue(reader)
              found(member.index) = true
              foundCount += 1

            case None =>
              reader.skipValue()
          }
        }

        reader.endObject()

        if (foundCount != numberOfMembers)
          for (member <- members if !found(member.index)) {
            arguments(member.index) = member.defaultValue.getOrElse(
              throw new IllegalStateException(s"Required field ${member.name} in class ${tpe.typeSymbol.fullName} is missing from input and has no specified default value\n" + reader.showError())
            )
          }

        val asBuilt = constructorMirror.apply(arguments: _*).asInstanceOf[T]
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

  override def member(memberName: MemberName): Option[Member] =
    membersByName.get(memberName)

  override def readMemberName(reader: Reader): MemberName =
    memberNameTypeAdapter.read(reader)

  override def writeMemberName(memberName: MemberName, writer: Writer): Unit =
    memberNameTypeAdapter.write(memberName, writer)

  override def instantiate(memberValues: Array[Any]): T =
    constructorMirror.apply(memberValues: _*).asInstanceOf[T]

}
