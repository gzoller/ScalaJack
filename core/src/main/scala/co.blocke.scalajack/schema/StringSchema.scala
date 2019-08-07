package co.blocke.scalajack
package schema

import org.scalactic._
import Accumulation._
import model._
import scala.util.matching.Regex

case class StringSchema(minLength: Option[Int], maxLength: Option[Int], pattern: Option[String] = None) extends Schema[String] {
  private val regex = pattern.map(_.r)

  def validate(
      value: String
  )(implicit tt: TypeTag[String]): Boolean Or Every[SJError] =
    withGood(
      check(
        minLength,
        (m: Int) => {
          if (value.length >= m) Good(true)
          else Bad(One(new SchemaValidationError(s"Given string was not at least the minimum (${minLength.get}) chars long.")))
        }
      ),
      check(
        maxLength,
        (m: Int) => {
          if (value.length <= m) Good(true)
          else Bad(One(new SchemaValidationError(s"Given string exceeded the maximum length of ${maxLength.get} chars.")))
        }
      ),
      check(
        regex,
        (r: Regex) => {
          if (regex.forall(r => r.findFirstIn(value).isDefined)) Good(true)
          else Bad(One(new SchemaValidationError(s"Given string does not match pattern ${pattern.get}")))
        }
      )
    ) { _ & _ & _ }
}
