package co.blocke.scalajack
package model

import scala.reflect.runtime.universe._
import java.lang.reflect.Method
import scala.reflect.ClassTag
import typeadapter._

object ClassHelper {

  type MemberName = String

  // Field values (w/TypeAdatper) that are "extra"--i.e. not part of a class but we want to read/render them along
  // with normal class fields.  For example: type member fields.
  case class ExtraFieldValue[T](value: T, valueTypeAdapter: TypeAdapter[T])

  sealed trait Member[Owner] {
    def name: String
  }

  def annotationExists[T](sym: Symbol)(implicit tt: TypeTag[T]): Boolean =
    sym.annotations.exists(_.tree.tpe =:= typeOf[T])

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
      defaultValueMethod:                 Option[Method], // <-- Need a Java Method here to work with Java classes too!
      outerClass:                         Option[java.lang.Class[_]],
      dbKeyIndex:                         Option[Int],
      fieldMapName:                       Option[String],
      ownerType:                          Type,
      // These 3 are only for Plain Classes -- unused for Case Classes
      valueSetterMethodSymbol: Option[MethodSymbol], // for Scala non-constructor setters
      valueSetterMethod:       Option[Method], // for Java beans setters
      hasOptionalAnnotation:   Boolean              = false
  ) extends Member[Owner] {
    type Value = T

    val defaultValue: Option[T] = defaultValueMethod
      .map(_.invoke(ownerType.typeSymbol.asClass).asInstanceOf[T])
      .orElse(valueTypeAdapter.defaultValue)

    lazy val isOptional: Boolean = valueTypeAdapter
      .isInstanceOf[OptionTypeAdapter[_]] || hasOptionalAnnotation

    // For Case Classes only
    def valueIn(owner: Owner): Value = {
      val value = valueAccessorMethod.invoke(owner).asInstanceOf[Value]
      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        value
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) =>
            methodMirror.apply(value).asInstanceOf[Value]

          case None =>
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            value
          // $COVERAGE-ON$
        }
      }
    }

    // For Plain Classes only
    def valueSet(instance: Owner, value: Value)(implicit
        tt: TypeTag[Owner],
                                                ct: ClassTag[Owner]): Unit =
      valueSetterMethodSymbol match {
        case Some(vsms) =>
          scala.reflect.runtime.currentMirror
            .reflect(instance)
            .reflectMethod(vsms)(value) // Scala
        case None =>
          valueSetterMethod.get
            .invoke(instance, value.asInstanceOf[Object]) // Java
      }
  }

  //
  //------ Helpful utilities relating to classes
  //

  // Picks up annotations for class and case class parameters
  def getAnnotationValue[T, U](sym: Symbol, default: Option[U] = None)(
      implicit
      tt: TypeTag[T]
  ): Option[U] = {
    val annotation = sym.annotations.find(_.tree.tpe =:= typeOf[T])
    annotation
      .flatMap { a =>
        if (a.tree.children.tail.isEmpty)
          default
        else
          a.tree.children.tail.head
            .collect({
              case Literal(Constant(value)) => value
            })
            .headOption
      }
      .asInstanceOf[Option[U]]
  }

  @inline def extractDefaultConstructorParamValueMethod(
      clazz:  Class[_],
      iParam: Int
  ): Option[Method] =
    clazz.getMethods.find(_.getName == "$lessinit$greater$default$" + iParam)

}
