package co.blocke.scalajack

case class Person(name: String, age: Int, flag: Option[Int], isOk: Option[Boolean], items: List[String])
trait Pet {
  val numLegs: Int
}
case class Dog(numLegs: Int) extends Pet

object Runme extends App {

  val sj = ScalaJack()

  import schema._

  val s = StringSchema(Some(5), Some(9), Some("^foo"))
  println(s.validate("foobx"))

  /*
  val c =
    ObjectSchema[Person](Some(3), None, None, None, None, None, None, None)(
      sj.context
    )
  val c2 =
    ObjectSchema[Pet](None, Some(1), None, None, None, None, None, None)(
      sj.context
    )
  val c3 =
    ObjectSchema[Person](None, None, Some(Array("name")), None, None, None, None, None)(
      sj.context
    )
  println(c.validate(Person("Greg", 53)))
  println(c2.validate(Dog(4)))
  println(c3.validate(Person("Greg", 53)))

  "null", "boolean", "object", "array", "number", or "string"), or "integer"

  val c4 =
    ObjectSchema[Person](
      None,
      None,
      None,
      None,
      Some(Map("^n.*" -> StringSchema(Some(3), None, None))),
      None,
      Some(Map("flag" -> Array("blather"))),
      None
    )(
      sj.context
    )
   */
  val c4 =
    ObjectSchema[Person](
      None,
      None,
      None,
      Some(Map("items" -> ArraySchema(None, None, None, None, None, None, None)(sj.context))),
      None,
      None,
      None,
      None
    )(
        sj.context
      )
  println(c4.validate(Person("Greg", 14, Some(5), Some(true), List("a", "b", "c"))))
}

/*
  maxProperties: Option[Int],
  minProperties: Option[Int],
  required: Option[Array[String]],
  properties: Option[Map[String, Schema[_]]], // Map[fieldName, Schema]
  patternProperties: Option[Map[String, Schema[_]]], // Map[regex, Schema]
  additionalProperties: Option[Either[Boolean, Schema[_]]],
  dependencies: Option[Map[String, Array[String]]], // "credit_card": ["billing_address"] (if credit_card field is present, billing_address is required
  propertyNames: Option[StringSchema],
  context: Context
 */

/*
{
	"$schema": "http://json-schema.org/draft-07/schema#",
    "properties": {
      "a": { "type":"string"},
      "a2": { "type": "integer"}
    },
    "patternProperties": {
      "^b": { "type": "boolean" }
    },
    "propertyNames": { "minLength": 1 },
    "additionalProperties": false
}
 */
