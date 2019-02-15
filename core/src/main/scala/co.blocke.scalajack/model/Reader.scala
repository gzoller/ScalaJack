package co.blocke.scalajack
package model

import util.Path

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.Map

case class ObjectFieldResult(
    allThere:   Boolean, // True if all fields found (Ok to create object)
    objectArgs: Array[Any], // Ordered arguments matching object constructor
    fieldSet:   Array[Boolean], // Bit map of fields set in case any missing
    captured:   Option[Map[String, Any]] = None // Captured fields if SJCapture
)

trait Reader[WIRE] {

  // Used for sub-parsing, e.g. Stringified Map keys or secondLookParsing
  def cloneWithSource(source: WIRE): Transceiver[WIRE]

  def savePos()
  def rollbackToSave()
  def peek(): TokenType.Value
  def lastTokenText(): String
  def skip()
  def lookAheadForTypeHint(fieldName: String, typeMaterializer: String => Type): Option[Type]

  def isDone(): Boolean

  def showError(): String

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To
  def readBigInt(path: Path): BigInt
  def readBoolean(path: Path): Boolean
  def readDecimal(path: Path): BigDecimal
  def readDouble(path: Path): Double
  def readInt(path: Path): Int
  def readLong(path: Path): Long
  def readMap[Key, Value, To](path: Path, canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To
  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldResult //(Boolean, Array[Any], Array[Boolean])
  def readString(path: Path): String
  def readTuple(path: Path, readFns: List[(Path, Transceiver[WIRE]) => Any]): List[Any]

}

