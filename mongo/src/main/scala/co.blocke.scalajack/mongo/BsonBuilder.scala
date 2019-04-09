package co.blocke.scalajack
package mongo

import org.bson.BsonValue
import scala.collection.mutable.Builder

case class BsonBuilder() extends Builder[BsonValue, BsonValue] {
  private var internalValue: Option[BsonValue] = None

  def +=(elem: BsonValue): this.type = {
    internalValue = Some(elem)
    this
  }

  def clear(): Unit = internalValue = None

  def result(): BsonValue = internalValue.getOrElse(throw new model.SJError("No value set for internal mongo builder"))
}
