package co.blocke.scalajack
package model

import scala.collection.generic.CanBuildFrom

trait Reader {
  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To
  def readBoolean(): Boolean
  def readDecimal(): BigDecimal
  def readDouble(): Double
  def readInt(): Int
  def readLong(): Long
  def readString(): String
}
