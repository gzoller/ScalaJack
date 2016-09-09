package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName

import co.blocke.scalajack.flexjson.typeadapter.CaseClassTypeAdapter.Member
import co.blocke.scalajack.flexjson.{ Context, EmptyReader, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type, typeOf }

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class Member[T](
      index:              Int,
      name:               String,
      valueTypeAdapter:   TypeAdapter[T],
      accessor:           MethodSymbol,
      defaultValueMirror: Option[MethodMirror]
  ) {

    def writeValue(parameterValue: Any, writer: Writer): Unit = {
      valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(parameterValue, writer)
    }

    def hasDefaultValue: Boolean = defaultValueMirror.isDefined

    def defaultValue: T = defaultValueMirror.get.apply().asInstanceOf[T]

  }

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type] = List.empty[Type]): Option[TypeAdapter[_]] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val typeBeforeSubstitution = constructorSymbol.infoIn(tpe)

      val typeAfterSubstitution =
        if (superParamTypes.isEmpty) {
          // tpe
          // typeBeforeSubstitution.substituteTypes(tpe.typeConstructor.typeParams, superParamTypes)
          typeBeforeSubstitution.substituteTypes(tpe.typeParams, tpe.typeParams.map(_ => typeOf[Any]))
        } else {
          typeBeforeSubstitution.substituteTypes(tpe.typeConstructor.typeParams, superParamTypes)
        }

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      Some(CaseClassTypeAdapter(typeAfterSubstitution, constructorMirror, tpe, context, companionType, companionMirror, memberNameTypeAdapter))
    } else {
      None
    }

}

case class CaseClassTypeAdapter[T](
    caseClassType:         Type,
    constructorMirror:     MethodMirror,
    tpe:                   Type,
    context:               Context,
    companionType:         Type,
    companionMirror:       reflect.runtime.universe.InstanceMirror,
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends TypeAdapter[T] {

  var ccMembers: Option[List[Member[_]]] = None
  def members = ccMembers.getOrElse(unpackMembers())

  // This code is here, not in the CaseClassTypeAdapter object, because we need to return the adapter immediately,
  // incase there are other threads trying to descend the members of this class, or self-references within the class.
  // This adapter, in turn, lazily reflects its members, which this time will find the CaseClassTypeAdapter, and populate
  // the member details.
  def unpackMembers(): List[Member[_]] = {
    if (ccMembers.isEmpty) {
      ccMembers = Some(caseClassType.paramLists.flatten.zipWithIndex.map({
        case (member, index) ⇒
          val memberName = member.name.encodedName.toString
          val accessor = tpe.member(TermName(memberName)).asMethod

          val defaultValueAccessor = companionType.member(TermName("apply$default$" + (index + 1)))
          val defaultValueAccessorMirror =
            if (defaultValueAccessor.isMethod) {
              Some(companionMirror.reflectMethod(defaultValueAccessor.asMethod))
            } else {
              None
            }

          Member(index, memberName, context.typeAdapter(member.info, member.info.typeArgs), accessor, defaultValueAccessorMirror)
      }))
    }
    ccMembers.get
  }

  lazy val membersByName = members.map(member ⇒ member.name → member.asInstanceOf[Member[Any]]).toMap

  override def read(reader: Reader): T = {
    val numberOfMembers = members.length

    val arguments = new Array[Any](numberOfMembers)
    val found = new mutable.BitSet(numberOfMembers)

    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[T]
    } else {
      reader.beginObject()

      while (reader.hasMoreMembers) {
        val memberName = memberNameTypeAdapter.read(reader)

        val optionalMember = membersByName.get(memberName)
        optionalMember match {
          case Some(member) ⇒
            arguments(member.index) = member.valueTypeAdapter.read(reader)
            found(member.index) = true

          case None ⇒
            reader.skipValue()
        }
      }

      var index = 0
      while (index < numberOfMembers) {
        if (!found(index)) {
          val member = members(index)
          member.defaultValueMirror match {
            case Some(mirror) ⇒
              arguments(index) = mirror.apply()

            case None ⇒
              arguments(index) = member.valueTypeAdapter.read(EmptyReader)
          }
        }

        index += 1
      }

      reader.endObject()

      constructorMirror.apply(arguments: _*).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit = {
    if (value == null)
      writer.writeNull()
    else {
      writer.beginObject()

      for (member ← members) {
        val classTag = ClassTag[T](value.getClass)
        val instanceMirror = currentMirror.reflect(value)(classTag)
        val accessorMirror = instanceMirror.reflectMethod(member.accessor)
        val memberValue = accessorMirror.apply()

        memberNameTypeAdapter.write(member.name, writer)
        member.writeValue(memberValue, writer)
      }

      writer.endObject()
    }
  }

}
