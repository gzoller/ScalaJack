package co.blocke.scalajack.compat

import co.blocke.scalajack.model
import org.json4s.JValue
import scala.collection.mutable.Builder

case class JValueBuilder() extends Builder[JValue, JValue] {
  private var internalValue: Option[JValue] = None

  def addOne(elem: JValue): this.type = {
    internalValue = Some(elem)
    this
  }

  def clear(): Unit = internalValue = None

  def result(): JValue = internalValue.getOrElse(throw new model.SJError("No value set for internal json4s builder"))
}
