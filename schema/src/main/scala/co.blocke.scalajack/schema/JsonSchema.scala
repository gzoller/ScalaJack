package co.blocke.scalajack
package schema

object SchemaType extends Enumeration {
  type SchemaType = Value

  val string, number, `object`, array, boolean, `null` = Value
}
import SchemaType._

trait Schema {
  def isValid[T](t: T)(implicit tt: TypeTag[T]): Boolean = true
}

case class JsonSchema(
    `type`: SchemaType,
    title:  String,
    `$id`:  Option[String] = None,
    `$schema`: Option[String] = Some(
      BuildInfo.jsonSchemaVersion
    ) // >>> DO NOT CHANGE <<< fixed for this Scalajack implementation
//    fieldMap: Map[String, SchemaEntry]
) extends Schema {
  //  def isValid[T](t: T)(implicit tt: TypeTag[T]): Boolean = true
}
