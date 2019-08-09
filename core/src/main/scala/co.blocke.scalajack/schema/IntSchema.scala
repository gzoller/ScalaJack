package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._

case class IntSchema(
    multipleOf:       Option[Long],
    maximum:          Option[Long],
    exclusiveMaximum: Option[Long],
    minimum:          Option[Long],
    exclusiveMinimum: Option[Long])
  extends Schema[Long] {

  val typeLabel = "integer"

  def validate(
      value:     Long,
      fieldName: Option[String] = None
  )(implicit tt: TypeTag[Long]): Boolean Or Every[SJError] = {
    val errField = fieldName.map(fn => s"(field $fn)--").getOrElse("")
    withGood(
      check(
        multipleOf,
        (m: Long) => {
          if (value % m == 0) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given integer is not a multiple of ${multipleOf.get}.")))
        }
      ),
      check(
        maximum,
        (m: Long) => {
          if (value <= m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given integer exceeded maximum value of ${maximum.get}.")))
        }
      ),
      check(
        exclusiveMaximum,
        (m: Long) => {
          if (value < m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given integer exceeded (exclusive) maximum value of ${exclusiveMaximum.get}.")))
        }
      ),
      check(
        minimum,
        (m: Long) => {
          if (value >= m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given integer was less than required minimum value of ${minimum.get}.")))
        }
      ),
      check(
        exclusiveMinimum,
        (m: Long) => {
          if (value > m) Good(true)
          else
            Bad(
              One(
                new SchemaValidationError(s"${errField}Given integer was less than required (exclusive) minimum value of ${exclusiveMinimum.get}.")))
        }
      )
    ) {
        _ & _ & _ & _ & _
      }
  }
}
