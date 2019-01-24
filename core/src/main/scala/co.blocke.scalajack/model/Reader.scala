package co.blocke.scalajack
package model

import util.Path

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Map

case class ObjectFieldResult(allThere: Boolean, objectArgs: Array[Any], fieldSet: Array[Boolean])

trait Reader[WIRE] {

  val tokenizer: Tokenizer[WIRE]

  // Used for sub-parsing, e.g. Stringified Map keys or secondLookParsing
  def cloneWithSource(source: WIRE): Transceiver[WIRE]

  def savePos()
  def rollbackToSave()
  def peek(): TokenType.Value
  def lastTokenText(): String
  def skip()
  def lookAheadForField(fieldName: String): Option[String]

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To
  def readBigInt(path: Path, isMapKey: Boolean): BigInt
  def readBoolean(path: Path, isMapKey: Boolean): Boolean
  def readDecimal(path: Path, isMapKey: Boolean): BigDecimal
  def readDouble(path: Path, isMapKey: Boolean): Double
  def readInt(path: Path, isMapKey: Boolean): Int
  def readLong(path: Path, isMapKey: Boolean): Long
  def readMap[Key, Value, To](path: Path, canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To
  def readObjectFields[T](path: Path, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): ObjectFieldResult //(Boolean, Array[Any], Array[Boolean])
  def readString(path: Path): String
}

