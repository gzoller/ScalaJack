package co.blocke.scalajackx
package json

import org.json4s._

class JsonAstOps() extends AstOps[JValue] {
  type ObjectType = JObject
  type ArrayType = JValue

  def applyArray(value: Seq[JValue]): JValue = JArray(value.toList)
  def unapplyArray(ast: JValue): Option[Seq[JValue]] = ast match {
    case j: JArray => Some(j.arr)
    case _         => None
  }

  def applyBoolean(value: Boolean): JValue = JBool(value)
  def unapplyBoolean(ast: JValue): Option[Boolean] = ast match {
    case j: JBool => Some(j.value)
    case _        => None
  }
}
