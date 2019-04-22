package co.blocke.scalajack
package compat

import scala.collection.mutable.Builder
import org.bson.BsonValue

case class BsonBuilder() extends Builder[BsonValue, BsonValue] {
  private var internalValue: Option[BsonValue] = None

  def addOne(elem: BsonValue): this.type = {
    internalValue = Some(elem)
    this
  }

  def clear(): Unit = internalValue = None

  def result(): BsonValue = internalValue.getOrElse(throw new model.SJError("No value set for internal mongo builder"))
}
