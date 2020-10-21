package co.blocke.scalajack
package mongo

import org.bson.BsonValue

import scala.collection.mutable

case class BsonBuilder() extends mutable.Builder[BsonValue, BsonValue] {
  private var internalValue: Option[BsonValue] = None

  def addOne(elem: BsonValue): this.type = {
    internalValue = Some(elem)
    this
  }

  def clear(): Unit = internalValue = None

  def result(): BsonValue =
    internalValue.getOrElse(
      throw new ScalaJackError("No value set for internal mongo builder")
    )
}