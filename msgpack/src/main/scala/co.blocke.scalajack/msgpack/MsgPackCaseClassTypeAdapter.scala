package co.blocke.scalajack
package msgpack

import java.lang.reflect.Method

import typeadapter.CaseClassTypeAdapter.Member
import typeadapter.ClassMember

import scala.collection.mutable
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, NoType, TermName, Type, typeOf }

object MsgPackCaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {
  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

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

          // Exctract DBKey annotation if present
          val dbkeyAnnotation = member.annotations.find(_.tree.tpe =:= typeOf[DBKey])
            .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
              .value().value).asInstanceOf[Option[Int]]

          val memberTypeAdapter = context.typeAdapter(memberType)
          Member(index, memberName, memberTypeAdapter, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass, dbkeyAnnotation)
      })

      // Exctract Collection name annotation if present
      val collectionAnnotation = classSymbol.annotations.find(_.tree.tpe =:= typeOf[Collection])
        .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
          .value().value).asInstanceOf[Option[String]]

      val dbKeys = members.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

      Some(MsgPackCaseClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members, members.length, isSJCapture, dbKeys, collectionAnnotation))
    } else {
      None
    }

}

case class MsgPackCaseClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[ClassMember[_]],
    numberOfMembers:       Int,
    isSJCapture:           Boolean,
    dbKeys:                List[ClassMember[_]],
    collectionName:        Option[String]          = None
) extends TypeAdapter[T] {

  val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[Member[Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.BeginObject ⇒
        val arguments = new Array[Any](numberOfMembers)
        val found = new Array[Boolean](numberOfMembers)
        var foundCount = 0

        reader.beginObject()

        var savedPos = reader.position
        while (reader.hasMoreMembers) {
          membersByName.get(memberNameTypeAdapter.read(reader)) match {
            case Some(member) ⇒
              arguments(member.index) = member.valueTypeAdapter.read(reader)
              found(member.index) = true
              foundCount += 1

            case None ⇒
              reader.skipValue()
          }
        }

        reader.endObject()

        if (foundCount != numberOfMembers)
          for (member ← members if !found(member.index)) {
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
              case Some(member) ⇒ reader.skipValue // do nothing... already built class
              case None ⇒
                captured.put(memberName, reader.captureValue())
            }
          }
          asBuilt.asInstanceOf[SJCapture].captured = captured
        }
        asBuilt

      case TokenType.Null ⇒
        reader.readNull()
    }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      val numMembers = members.foldLeft(0)((acc, m) => if (m.valueIn(value) == None) acc else acc + 1) + {
        value match {
          case sjc: SJCapture ⇒ sjc.captured.size
          case _              ⇒ 0
        }
      }
      writer.asInstanceOf[MsgPackWriterLike].beginObject(numMembers)

      for (member ← members) {
        val memberValue = member.valueIn(value)
        if (memberValue != None) {
          memberNameTypeAdapter.write(member.name, writer)
          member.writeValue(memberValue, writer)
        }
      }
      value match {
        case sjc: SJCapture ⇒
          sjc.captured.foreach {
            case (memberName, valueString) ⇒
              memberNameTypeAdapter.write(memberName, writer)
              writer.writeRawValue(valueString.asInstanceOf[String])
          }
        case _ ⇒
      }

      writer.endObject()
    }

}
