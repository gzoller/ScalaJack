package co.blocke.scalajack
package model

import util.Path

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Map

trait Reader {
  type WIRE
  val tokenizer: Tokenizer[WIRE]

  def cloneWithSource(source: WIRE): Reader // used for Any parsing

  def savePos()
  def rollbackToSave()
  def peek(): TokenType.Value
  def tokenText(): String
  def skip()
  def lookAheadForField(fieldName: String): Option[String]

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To
  def readBigInt(isMapKey: Boolean): BigInt
  def readBoolean(isMapKey: Boolean): Boolean
  def readDecimal(isMapKey: Boolean): BigDecimal
  def readDouble(isMapKey: Boolean): Double
  def readInt(isMapKey: Boolean): Int
  def readLong(isMapKey: Boolean): Long
  def readMap[Key, Value, To](path: Path, canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To
  def readObjectFields[T](path: Path, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): (Boolean, Array[Any], Array[Boolean])
  def readString(): String
}
