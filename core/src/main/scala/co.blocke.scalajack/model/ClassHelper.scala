package co.blocke.scalajack
package model

import java.lang.reflect.Method

import typeadapter.{ FallbackTypeAdapter, OptionTypeAdapter }

import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object ClassHelper {

  sealed trait Member[Owner] {
    def name: String
  }

  case class TypeMember[Owner](
      name:                MemberName, // name of the type
      typeSignature:       Type, // signature (i.e. the generic letter 'T', e.g. Foo[T]
      baseType:            Type, // defined type (likely a trait)
      runtimeConcreteType: Option[Type] = None // inferred concrete type reflecting on actual class (or materialized from input)
  ) extends Member[Owner]

  case class ClassFieldMember[Owner, T](
      index:                              Int,
      name:                               MemberName,
      valueType:                          Type,
      valueTypeAdapter:                   TypeAdapter[T],
      declaredValueType:                  Type,
      valueAccessorMethod:                Method,
      derivedValueClassConstructorMirror: Option[MethodMirror],
      defaultValueMethod:                 Option[Method],
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      fieldMapName:                       Option[String],

      // These 4 are only for Plain Classes -- unused for Case Classes
      valueSetterMethodSymbol: Option[MethodSymbol], // for Scala
      valueSetterMethod:       Option[Method], // for Java
      isIgnore: Boolean = false,
      isMaybe: Boolean = false
  )(implicit ot:TypeTag[Owner]) extends Member[Owner] {
    type Value = T

    val defaultValue: Option[T] = defaultValueMethod.map(_.invoke(ot.tpe.typeSymbol.asClass).asInstanceOf[T]).orElse(valueTypeAdapter.defaultValue)

    lazy val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    // For Case Classes only
    def valueIn(owner: Owner): Value = {
      val value = valueAccessorMethod.invoke(owner).asInstanceOf[Value]
      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) => methodMirror.apply(value).asInstanceOf[Value]

          case None =>
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            value
          // $COVERAGE-ON$
        }
      }
    }

    // For Plain Classes only
    def valueSet(instance: Owner, value: Value)(implicit tt: TypeTag[Owner], ct: ClassTag[Owner]): Unit =
      valueSetterMethodSymbol match {
        case Some(vsms) =>
          scala.reflect.runtime.currentMirror.reflect(instance).reflectMethod(vsms)(value) // Scala
        case None       =>
          valueSetterMethod.get.invoke(instance, value.asInstanceOf[Object])  // Java
      }
  }

  //----- Helpful Utilities ----

  // Picks up annotations for class and case class parameters
  def getAnnotationValue[T, U](sym: Symbol, default: Option[U] = None)(implicit tt: TypeTag[T]): Option[U] = {
    val annotation = sym.annotations.find(_.tree.tpe =:= typeOf[T])
    annotation.flatMap { a =>
      if (a.tree.children.tail.size == 0)
        default
      else
        a.tree.children.tail.head.collect({
          case Literal(Constant(value)) => value
        }).headOption
    }.asInstanceOf[Option[U]]
  }

  @inline def extractDefaultConstructorParamValueMethod(clazz: Class[_], iParam: Int): Option[Method] =
    clazz.getMethods().find(_.getName == "$lessinit$greater$default$" + iParam)

  def annotationExists[T](sym: Symbol)(implicit tt: TypeTag[T]) =
    sym.annotations.find(_.tree.tpe =:= typeOf[T]).isDefined

  trait ClassLikeTypeAdapter[C] extends TypeAdapter[C] {
    val className: String
    val typeMembersByName: Map[String, TypeMember[C]]
    val fieldMembersByName: ListMap[String, ClassFieldMember[C, Any]]
    val collectionName: Option[String]

    def dbKeys: List[ClassFieldMember[C, Any]] = fieldMembersByName.values.toList.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)
    def members = typeMembersByName.values ++ fieldMembersByName.values
  }

  case class ExtraFieldValue[T](
      value:            T,
      valueTypeAdapter: TypeAdapter[T]) {
    def write[WIRE](writer: Transceiver[WIRE], out: Builder[Any, WIRE]) = valueTypeAdapter.write(value, writer, out, false)
  }

  // OK, so all this hokem is to figure out what to do for embedded type member (i.e. externalized type hint feature).
  // *If* there are interesting type members for this class, find fields in the class with types matching the type class and substitute the
  // actual concrete types (and corresponding type adapters) for the placeholder types, T.
  def applyConcreteTypeMembersToFields[T](
      concreteTypes: Map[String,TypeMember[_]],
      typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
      fieldMembersByName: ListMap[String, ClassFieldMember[T, Any]],
  )(implicit context: Context): ListMap[String, ClassHelper.ClassFieldMember[T, Any]] = {

    if (concreteTypes.isEmpty) {
      fieldMembersByName
    } else {
      // If type members are defined --> externalized trait concrete type
      // Create a mapping of type label, e.g. 'T', to TypeMember where we've resolved the type member's value into a Type

      // Now buzz through known field members and replaces all the 'T' type with the concrete type and insert the correct concrete TypeAdapter.
      fieldMembersByName.map {
        case (name, field) =>
          val findConcrete = concreteTypes.get(field.declaredValueType.toString) match {
            case Some(c) =>
              val runtimeTypeAdapter = c.runtimeConcreteType.map(context.typeAdapter(_))
              val newTypeAdapter = field.valueTypeAdapter match {
                case falling: FallbackTypeAdapter[_, _] =>
                  FallbackTypeAdapter(runtimeTypeAdapter.asInstanceOf[Option[TypeAdapter[Any]]], falling.orElseTypeAdapter)
                case _ =>
                  runtimeTypeAdapter.getOrElse(throw new IllegalStateException("Can't find type value (e.g. unknown class) for hint " + name))
              }
              field.copy(
                valueTypeAdapter  = newTypeAdapter,
                declaredValueType = c.runtimeConcreteType.getOrElse(field.declaredValueType)
              ).asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
            case None =>
              field
          }
          (name, findConcrete)
      }
    }
  }
}
