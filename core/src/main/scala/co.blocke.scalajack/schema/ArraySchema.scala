package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._
import typeadapter.classes._
import scala.reflect.runtime.universe._
import scala.reflect.api
import scala.reflect.runtime.universe

case class ArraySchema[T](
    items:           Option[Either[Schema[_], Array[Schema[_]]]],
    additionalItems: Option[Schema[_]],
    maxItems:        Option[Int],
    minItems:        Option[Int],
    uniqueItems:     Option[Boolean],
    contains:        Option[Schema[_]],
    description:     Option[String]                              = None
)(implicit context: Context)
  extends Schema[T] {

  val typeLabel = "object"

  val mirror: universe.Mirror = runtimeMirror(getClass.getClassLoader) // whatever mirror you use to obtain the `Type`

  def validate(value: T, fieldName: Option[String] = None)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    val errField = fieldName.map(fn => s"(field $fn)--").getOrElse("")
    val values = value match {
      case a: Array[_]                        => a.toList
      case x: scala.collection.AbstractSeq[_] => x.toList
      case _                                  => ???
    }
    val ta = context.typeAdapterOf[T]
    //    val fieldMember    = ta.fieldMembersByName(fieldName)
    //    val realValue      = fieldMember.valueIn(objValue)
    //    val castValue      = realValue.asInstanceOf[fieldMember.Value]
    //    val realType: Type = runtimeMirror(realValue.getClass.getClassLoader).classSymbol(realValue.getClass).toType
    println(values)
    println(tt.tpe)
    println(ta)
    itemChecks(value)
  }

  private def itemChecks(value: T)(implicit tt: TypeTag[T]): Boolean Or Every[SJError] = {
    /*
    val values =
      items
        .map {
          _ match {
            case Left(s) => Good(true)
            case Right(arrS) if arrS.length != valuesIn.length =>
              Bad(One(new SchemaValidationError("Given array's length is not the same as schema 'items' spec length.")))
            case Right(arrS) =>
              val goo = valuesIn.zip(arrS).map { case (thing, schema) => schema.validate(thing) }
          }
        }
        .getOrElse(Good(true))
     */
    Good(true)
  }
}
