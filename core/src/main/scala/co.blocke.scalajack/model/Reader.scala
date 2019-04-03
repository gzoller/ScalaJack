package co.blocke.scalajack
package model

import util.Path

import scala.collection.immutable.ListMap
import typeadapter.TupleTypeAdapterFactory

trait Transceiver[WIRE] {
  val jackFlavor: JackFlavor[WIRE]
}

trait Reader[WIRE] extends collection.BufferedIterator[ParseToken[WIRE]] with Transceiver[WIRE] {

  // Use this to "save" current state into a copy in case you need to revert
  def copy: Reader[WIRE]
  def syncPositionTo(reader: Reader[WIRE]): Unit // "merge" state with given reader

  // Pre-scan input looking for given hint label.  Should not change the parser's state (pointer)
  def scanForHint(hintLabel: String): Option[String]
  def scanForType(path: Path, hintLabel: String, hintModFn: Option[HintValueModifier]): Option[Type]

  // BufferedIterator controls
  def hasNext: Boolean
  def head: ParseToken[WIRE]
  def next: ParseToken[WIRE] // skip over next token
  def back: ParseToken[WIRE]
  def reset(): Unit

  // Print a clip from the input and a grapical pointer to the problem for clarity
  def showError(path: Path, msg: String): String

  // Read Primitives
  def readBigInt(path: Path): BigInt
  def readBoolean(path: Path): Boolean
  def readDecimal(path: Path): BigDecimal
  def readDouble(path: Path): Double
  def readInt(path: Path): Int
  def readLong(path: Path): Long
  def readString(path: Path): String

  // Read Basic Collections
  def readArray[Elem, To](path: Path, builderFactory: MethodMirror, elementTypeAdapter: TypeAdapter[Elem]): To
  def readMap[Key, Value, To](path: Path, builderFactory: MethodMirror, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To
  def readTuple(path: Path, readFns: List[TupleTypeAdapterFactory.TupleField[_]]): List[Any]

  // Read fields we know to be object fields
  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead //(Boolean, Array[Any], Array[Boolean])
  def skipObject(path: Path): Unit
}

