package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import CaseClassTypeAdapter.Member
import PlainClassTypeAdapter.PlainMember

import scala.util.{ Try, Success, Failure }
import scala.collection.mutable
import scala.language.{ existentials, reflectiveCalls }
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import java.beans.Introspector

// WARNING: This adapter should be last in the list!  This classSymbol.isClass will match pretty much
// anything all the other adapters before it failed to match, so nothing after this adapter will be
// visible/matchable!

object PlainClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class PlainMember[T](
      index:                              Int,
      name:                               String,
      valueTypeAdapter:                   TypeAdapter[T],
      valueGetterMethod:                  Method,
      valueSetterMethod:                  Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]]
  ) extends ClassMember[T] {

    val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    def valueIn(instance: Any): T = {
      val value = valueGetterMethod.invoke(instance)

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

    def valueSet(instance: Any, value: Object): Unit = valueSetterMethod.invoke(instance, value)

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    def defaultValue: Option[T] = None

  }

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isClass) {

      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)
      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]
      val isSJCapture = !(tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      def inferConstructorValFields: List[ClassMember[_]] =
        Try { // Might fail if *all* the constructor's parameters aren't val notated
          constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.zipWithIndex.map({
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

              val memberType = member.asTerm.typeSignature
              val memberTypeAdapter = context.typeAdapter(memberType)
              Member(index, memberName, memberTypeAdapter, accessorMethod, derivedValueClassConstructorMirror, None, memberClass)
          })
        } match {
          case Success(m) ⇒ m
          case Failure(x) ⇒ List.empty[ClassMember[_]]
        }

      def reflectScalaGetterSetterFields: List[ClassMember[_]] = {
        tpe.members.filter(p ⇒ p.isPublic && p.isMethod).collect {
          // Scala case
          case p if (tpe.member(TermName(p.name.toString + "_$eq")) != NoSymbol) ⇒
            val memberType = p.asMethod.returnType
            val memberTypeAdapter = context.typeAdapter(memberType)
            PlainMember(
              0,
              p.name.encodedName.toString,
              memberTypeAdapter,
              Reflection.methodToJava(p.asMethod),
              Reflection.methodToJava(tpe.member(TermName(p.name.toString + "_$eq")).asMethod),
              None,
              None
            )
        }.toList.zipWithIndex.map { case (pm, index) ⇒ pm.copy(index = index) }
      }

      def reflectJavaGetterSetterFields: List[ClassMember[_]] = {
        val clazz = currentMirror.runtimeClass(tpe.typeSymbol.asClass)
        Introspector.getBeanInfo(clazz).getPropertyDescriptors().toList.filterNot(_.getName == "class").map { propertyDescriptor ⇒
          val memberType = tpe.member(TermName(propertyDescriptor.getReadMethod.getName)).asMethod.returnType
          val memberTypeAdapter = context.typeAdapter(memberType)
          PlainMember(
            0,
            propertyDescriptor.getName,
            memberTypeAdapter,
            propertyDescriptor.getReadMethod,
            propertyDescriptor.getWriteMethod,
            None,
            None
          )
        }.zipWithIndex.map { case (pm, index) ⇒ pm.copy(index = index) }
      }

      val hasEmptyConstructor = constructorSymbol.typeSignatureIn(tpe).paramLists.flatten.isEmpty
      inferConstructorValFields match {
        case members if (!members.isEmpty) ⇒
          // Because all the val fields were found in the constructor we can use a normal CaseClassTypeAdapter
          Some(CaseClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture))
        case _ if (!classSymbol.isJava && hasEmptyConstructor) ⇒
          val members = reflectScalaGetterSetterFields
          Some(PlainClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture))
        case _ if (classSymbol.isJava && hasEmptyConstructor) ⇒
          val members = reflectJavaGetterSetterFields
          Some(PlainClassTypeAdapter(tpe, constructorMirror, tpe, memberNameTypeAdapter, members, isSJCapture))
        case x =>
          println("BOOM: " + x)
          None
        // case _ ⇒
        //   throw new Exception("Must have 0 params!")
      }

    } else
      None
}

case class PlainClassTypeAdapter[T >: Null](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    memberNameTypeAdapter: TypeAdapter[MemberName],
    members:               List[ClassMember[_]],
    isSJCapture:           Boolean
) extends TypeAdapter[T] {

  val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[ClassMember[Any]]).toMap

  override def read(reader: Reader): T =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.BeginObject ⇒
        val numberOfMembers = members.length

        val found = new mutable.BitSet(numberOfMembers)

        reader.beginObject()

        val asBuilt = constructorMirror.apply().asInstanceOf[T] // call 0-parameter constructor

        val captured = scala.collection.mutable.Map.empty[String, Any]
        while (reader.hasMoreMembers) {
          val memberName = memberNameTypeAdapter.read(reader)

          val optionalMember = membersByName.get(memberName)
          optionalMember match {
            case Some(member) ⇒
              member.asInstanceOf[PlainMember[_]].valueSet(asBuilt, member.valueTypeAdapter.read(reader).asInstanceOf[Object])
              found(member.index) = true

            case None if (isSJCapture) ⇒
              captured.put(memberName, reader.captureValue())

            case None ⇒
              reader.skipValue()
          }
        }

        reader.endObject()

        for (member ← members if !found(member.index)) {
          throw new IllegalStateException(s"Required field ${member.name} in class ${tpe.typeSymbol.fullName} is missing from input and has no specified default value\n" + reader.showError())
        }

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
