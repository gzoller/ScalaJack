package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._

case class NumberSchema(
    multipleOf:       Option[Double],
    maximum:          Option[Double],
    exclusiveMaximum: Option[Double],
    minimum:          Option[Double],
    exclusiveMinimum: Option[Double])
  extends Schema[Double] {

  val typeLabel = "number"

  def validate(
      value:     Double,
      fieldName: Option[String] = None
  )(implicit tt: TypeTag[Double]): Boolean Or Every[SJError] = {
    val errField = fieldName.map(fn => s"(field $fn)--").getOrElse("")
    withGood(
      check(
        multipleOf,
        (m: Double) => {
          if (value % m == 0) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given number is not a multiple of ${multipleOf.get}.")))
        }
      ),
      check(
        maximum,
        (m: Double) => {
          if (value <= m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given number exceeded maximum value of ${maximum.get}.")))
        }
      ),
      check(
        exclusiveMaximum,
        (m: Double) => {
          if (value < m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given number exceeded (exclusive) maximum value of ${exclusiveMaximum.get}.")))
        }
      ),
      check(
        minimum,
        (m: Double) => {
          if (value >= m) Good(true)
          else Bad(One(new SchemaValidationError(s"${errField}Given number was less than required minimum value of ${minimum.get}.")))
        }
      ),
      check(
        exclusiveMinimum,
        (m: Double) => {
          if (value > m) Good(true)
          else
            Bad(
              One(new SchemaValidationError(s"${errField}Given number was less than required (exclusive) minimum value of ${exclusiveMinimum.get}.")))
        }
      )
    ) {
        _ & _ & _ & _ & _
      }
  }
}
