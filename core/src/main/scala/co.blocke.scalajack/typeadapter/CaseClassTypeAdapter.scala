package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import scala.language.existentials
import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }

trait ClassFieldMember[Owner] extends ClassLikeTypeAdapter.FieldMember[Owner] {
  def dbKeyIndex: Option[Int]
  def declaredValueType: Type
}

object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  case class TypeMember[Owner](name: MemberName, typeSignature: Type, baseType: Type) extends ClassLikeTypeAdapter.TypeMember[Owner]

  case class FieldMember[Owner, T](
      index:                              Int,
      name:                               MemberName,
      valueType:                          Type,
      valueTypeAdapter:                   TypeAdapter[T],
      declaredValueType:                  Type,
      valueAccessorMethodSymbol:          MethodSymbol,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMirror:                 Option[MethodMirror],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      fieldMapName:                       Option[String],
      annotations:                        List[universe.Annotation]) extends ClassFieldMember[Owner] {

    override type Value = T

    override val valueTypeTag = new TypeTag[T] {

      // $COVERAGE-OFF$Unused in our context
      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[T] = ???
      // $COVERAGE-ON$

      override val mirror: universe.Mirror = currentMirror

      override def tpe: universe.Type = valueType

    }

    override def valueIn(tagged: TypeTagged[Owner]): TypeTagged[Value] = {
      val TypeTagged(owner) = tagged
      val value = valueAccessorMethod.invoke(owner)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        TypeTagged(value.asInstanceOf[Value], valueType)
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) =>
            TypeTagged(methodMirror.apply(value).asInstanceOf[Value], valueType)

          case None =>
            TypeTagged(value.asInstanceOf[Value], valueType)
        }
      }
    }

    // Find any specified default value for this field.  If none...and this is an Optional field, return None (the value)
    // otherwise fail the default lookup.
    override def defaultValue: Option[Value] =
      defaultValueMirror.map(_.apply().asInstanceOf[T]).orElse(valueTypeAdapter.defaultValue)

    override def deserializeValueFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[T] =
      valueTypeAdapter.deserializer.deserializeFromNothing(path)

    override def deserializeValue[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] =
      valueTypeAdapter.deserializer.deserialize(path, json)

    override def serializeValue[J](tagged: TypeTagged[T])(implicit ops: JsonOps[J]): SerializationResult[J] =
      valueTypeAdapter.serializer.serialize(tagged)

    override def annotationOf[A](implicit tt: TypeTag[A]): Option[universe.Annotation] =
      annotations.find(_.tree.tpe =:= tt.tpe)

    override def isStringValue: Boolean =
      valueTypeAdapter.isInstanceOf[StringKind]

  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = currentMirror.reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = currentMirror.reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = currentMirror.reflect(companionObject)

      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]

      val isSJCapture = !(tt.tpe.baseType(typeOf[SJCapture].typeSymbol) == NoType)

      val tm = tt.tpe.members.filter(_.isType).toList
      val classTypeParamMap = tt.tpe.typeSymbol.asClass.typeParams.zip(tt.tpe.typeArgs).toMap
      val typeMembers = tm map { m =>
        TypeMember[T](m.name.decodedName.toString, m.typeSignature, classTypeParamMap(m.typeSignature.typeSymbol))
      }

      val params1 = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten
      val params2 = constructorSymbol.typeSignatureIn(tt.tpe.typeSymbol.asType.toType).paramLists.flatten

      val fieldMembers = for (((member, param2), index) <- (params1 zip params2).zipWithIndex) yield {
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

        val declaredMemberType = param2.asTerm.typeSignature

        // Exctract DBKey annotation if present
        val optionalDbKeyIndex = member.annotations.find(_.tree.tpe =:= typeOf[DBKey])
          .map { index =>
            if (index.tree.children.size > 1)
              index.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal].value().value
            else
              0
          }.asInstanceOf[Option[Int]]

        // Extract MapName annotation if present
        val optionalMapName: Option[String] = member.annotations.find(_.tree.tpe =:= typeOf[MapName])
          .map { index =>
            index.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal].value().value
          }.asInstanceOf[Option[String]]

        val memberTypeAdapter = context.typeAdapter(memberType).asInstanceOf[TypeAdapter[Any]]
        FieldMember[T, Any](index, optionalMapName.getOrElse(memberName), memberType, memberTypeAdapter, declaredMemberType, accessorMethodSymbol, accessorMethod, derivedValueClassConstructorMirror, defaultValueAccessorMirror, memberClass, optionalDbKeyIndex, optionalMapName, member.annotations)
      }

      // Exctract Collection name annotation if present
      val collectionAnnotation = classSymbol.annotations.find(_.tree.tpe =:= typeOf[Collection])
        .map(_.tree.children(1).productElement(1).asInstanceOf[scala.reflect.internal.Trees$Literal]
          .value().value).asInstanceOf[Option[String]]

      CaseClassTypeAdapter[T](
        new ClassDeserializerUsingReflectedConstructor[T](
          context,
          constructorMirror,
          context.typeAdapterOf[Type].deserializer,
          typeMembers,
          fieldMembers,
          isSJCapture),
        new ClassSerializer[T](
          context,
          constructorMirror,
          context.typeAdapterOf[Type].serializer,
          typeMembers,
          fieldMembers,
          isSJCapture),
        context,
        tt.tpe,
        constructorMirror,
        memberNameTypeAdapter,
        context.typeAdapterOf[Type],
        typeMembers,
        fieldMembers,
        isSJCapture,
        collectionAnnotation)
    } else {
      next.typeAdapterOf[T]
    }

}

case class CaseClassTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    context:                   Context,
    tpe:                       Type,
    constructorMirror:         MethodMirror,
    memberNameTypeAdapter:     TypeAdapter[MemberName],
    typeTypeAdapter:           TypeAdapter[Type],
    typeMembers:               List[CaseClassTypeAdapter.TypeMember[T]],
    fieldMembers:              List[ClassFieldMember[T]],
    isSJCapture:               Boolean,
    collectionName:            Option[String]                           = None) extends ClassLikeTypeAdapter[T] {

  val dbKeys: List[ClassFieldMember[T]] = fieldMembers.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

  private val typeMembersByName = typeMembers.map(member => member.name -> member).toMap
  private val fieldMembersByName = fieldMembers.map(member => member.name -> member).toMap

  // $COVERAGE-OFF$Not used for JSON (Mongo)
  override def typeMember(memberName: MemberName): Option[TypeMember] =
    typeMembersByName.get(memberName)

  override def fieldMember(memberName: MemberName): Option[FieldMember] =
    fieldMembersByName.get(memberName)

  override def instantiate(memberValues: Array[Any]): T =
    constructorMirror.apply(memberValues: _*).asInstanceOf[T]
  // $COVERAGE-ON$
}