package co.blocke.scalajack
package schema

import org.scalactic._
import model._

case class BooleanSchema(description: Option[String] = None) extends Schema[Boolean] {

  val typeLabel = "boolean"

  def validate(
      value:     Boolean,
      fieldName: Option[String] = None
  )(implicit tt: TypeTag[Boolean]): Boolean Or Every[SJError] = Good(true)
}
