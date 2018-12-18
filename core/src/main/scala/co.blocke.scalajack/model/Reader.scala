package co.blocke.scalajack
package model

import scala.collection.generic.CanBuildFrom

trait Reader {
  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To
  def readMap[Key, Value, To](canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To
  def readBoolean(): Boolean
  def readDecimal(): BigDecimal
  def readDouble(): Double
  def readInt(): Int
  def readLong(): Long
  def readString(): String
}
