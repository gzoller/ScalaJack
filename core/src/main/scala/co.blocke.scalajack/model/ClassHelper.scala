package co.blocke.scalajack
package model

import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.OptionTypeAdapter

import scala.collection.immutable.ListMap
import scala.collection.mutable.Builder
import scala.reflect.runtime.universe._

object ClassHelper {

  sealed trait Member[Owner] {
    def name: String
  }

  case class TypeMember[Owner](name: MemberName, typeSignature: Type, baseType: Type) extends Member[Owner]

  trait FieldMember[Owner] extends Member[Owner] {

    type Value

    // Case class and Plain class
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]
    val defaultValue: Option[Value]
    val valueAccessorMethod: Method
    val dbKeyIndex: Option[Int]

    def declaredValueType: Type

    lazy val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

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
  }

  case class ClassFieldMember[Owner, T](
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
      annotations:                        List[Annotation]) extends FieldMember[Owner] {

    type Value = T

    val defaultValue: Option[T] = defaultValueMirror.map(_.apply().asInstanceOf[T]).orElse(valueTypeAdapter.defaultValue)
  }

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

  trait ClassLikeTypeAdapter[C] extends TypeAdapter[C] {
    val className: String
    val typeMembers: List[TypeMember[C]]
    val fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[C, Any]]
    //    val fieldMembers: List[FieldMember[C]]
    val collectionName: Option[String]
    def dbKeys: List[FieldMember[C]] = fieldMembers.values.toList.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)
    def members = typeMembers ++ fieldMembers.values
  }

  case class ExtraFieldValue[T](
      value:            T,
      valueTypeAdapter: TypeAdapter[T]
  ) {
    def write[WIRE](writer: Transceiver[WIRE], out: Builder[Any, WIRE]) = valueTypeAdapter.write(value, writer, out)
  }
}
