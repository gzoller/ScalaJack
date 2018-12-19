package co.blocke.scalajack
package model

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ ListMap, Map }

trait Reader {
  type WIRE
  val tokenizer: Tokenizer[WIRE]

  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To
  def readBoolean(isMapKey: Boolean): Boolean
  def readDecimal(isMapKey: Boolean): BigDecimal
  def readDouble(isMapKey: Boolean): Double
  def readInt(isMapKey: Boolean): Int
  def readLong(isMapKey: Boolean): Long
  def readMap[Key, Value, To](canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To
  def readObjectFields[T](fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): Map[String, Any]
  def readString(): String
}
