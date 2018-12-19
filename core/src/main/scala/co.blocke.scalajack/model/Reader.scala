package co.blocke.scalajack
package model

import scala.collection.generic.CanBuildFrom

trait Reader {
  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To
  def readMap[Key, Value, To](canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To
  def readBoolean(isMapKey: Boolean): Boolean
  def readDecimal(isMapKey: Boolean): BigDecimal
  def readDouble(isMapKey: Boolean): Double
  def readInt(isMapKey: Boolean): Int
  def readLong(isMapKey: Boolean): Long
  def readString(): String
}
