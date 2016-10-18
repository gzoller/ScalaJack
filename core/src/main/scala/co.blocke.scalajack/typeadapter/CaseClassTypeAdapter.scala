package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import CaseClassTypeAdapter.Member

import scala.collection.mutable
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, NoType, TermName, Type, typeOf }

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[T](
      index:                              Int,
      name:                               String,
      valueTypeAdapter:                   TypeAdapter[T],
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int]
  ) {

    val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    def valueIn(instance: Any): T = {
      val value = valueAccessorMethod.invoke(instance)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value.asInstanceOf[T]
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) ⇒
            methodMirror.apply(value).asInstanceOf[T]

          case None ⇒
            value.asInstanceOf[T]
        }
      }
    }

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    def defaultValue: Option[T] =
      defaultValueMirror.map(_.apply().asInstanceOf[T]).orElse(if (isOptional) { Some(None).asInstanceOf[Option[T]] } else None)

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

      Some(CaseClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture, dbKeys, collectionAnnotation))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[Member[_]],
    isSJCapture:           Boolean,
    dbKeys:                List[Member[_]],
    collectionName:        Option[String]          = None
) extends TypeAdapter[T] {

  val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[Member[Any]]).toMap

  def getDbKeys() = dbKeys

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        val numberOfMembers = members.length

        val arguments = new Array[Any](numberOfMembers)
        val found = new mutable.BitSet(numberOfMembers)

        reader.beginObject()

        val captured = scala.collection.mutable.Map.empty[String, Any]
        while (reader.hasMoreMembers) {
          val memberName = memberNameTypeAdapter.read(reader)

          val optionalMember = membersByName.get(memberName)
          optionalMember match {
            case Some(member) ⇒
              arguments(member.index) = member.valueTypeAdapter.read(reader)
              found(member.index) = true

            case None if (isSJCapture) ⇒
              captured.put(memberName, reader.captureValue())

            case None ⇒
              reader.skipValue()
          }
        }

        reader.endObject()

        for (member ← members if !found(member.index)) {
          arguments(member.index) = member.defaultValue.getOrElse(
            throw new IllegalStateException(s"Required field ${member.name} in class ${tpe.typeSymbol.fullName} is missing from input and has no specified default value\n" + reader.showError())
          )
        }

        // constructorMirror.apply(arguments: _*).asInstanceOf[T]
        val asBuilt = constructorMirror.apply(arguments: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = captured
        asBuilt
    }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member ← members) {
        val memberValue = member.valueIn(value)

        memberNameTypeAdapter.write(member.name, writer)
        member.writeValue(memberValue, writer)
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
