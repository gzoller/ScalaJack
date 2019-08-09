package co.blocke.scalajack
package schema

import org.scalactic._
import model._

trait Schema[T] {
  val typeLabel: String
  def validate(value: T, fieldName: Option[String] = None)(implicit tt: TypeTag[T]): Boolean Or Every[SJError]

  protected def check[T](test: Option[T], fn: T => Boolean Or One[SJError]): Boolean Or One[SJError] =
    test.map(t => fn(t)).getOrElse(Good(true))

}

case class EmptySchema() extends Schema[Any] {
  val typeLabel = "null"
  def validate(value: Any, fieldName: Option[String] = None)(
      implicit
      tt: TypeTag[Any]
  ): Boolean Or Every[SJError] = Good(true)
}

/*



 */
/*
propertyNames is a schema that all of an object's properties must be valid against. Let's take a look at less confusing example.

{
  "type": "object",
  "propertyNames": { "maxLength": 3, "minLength": 3 },
  "patternProperties": {
    "": { "type": "number" }
  }
}
This describes an object where all properties names must be of length 3 and all property values must be numbers. Here's an example.

{
  "usd": 1,
  "eur": 0.86815,
  "gbp": 0.76504,
  "cad": "1.31004",  <= Invalid property value
  "xx": 1.11         <= Invalid property name
}



The additionalProperties keyword is used to control the handling of extra stuff, that is, properties whose names are
not listed in the properties keyword. By default any additional properties are allowed.
The additionalProperties keyword may be either a boolean or an object. If additionalProperties is a boolean
and set to false, no additional properties will be allowed.
 */
