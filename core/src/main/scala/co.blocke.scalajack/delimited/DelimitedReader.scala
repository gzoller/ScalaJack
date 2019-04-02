package co.blocke.scalajack
package delimited

import model._
import json.JsonToken
import util.Path
import java.util.ArrayList
import java.lang.{ UnsupportedOperationException => UOE }

import co.blocke.scalajack.typeadapter.classes
import co.blocke.scalajack.typeadapter.classes.PlainClassTypeAdapter

import scala.collection.mutable.Builder
import scala.collection.immutable.{ ListMap, Map }

case class DelimitedReader(jackFlavor: JackFlavor[String], delimited: String, tokens: ArrayList[JsonToken], initialPos: Int = 0) extends Reader[String] {

  private var pos = initialPos
  private var currentField: Option[ClassHelper.ClassFieldMember[_, Any]] = None

  // Use this to "save" current state into a copy in case you need to revert
  def copy: Reader[String] = DelimitedReader(jackFlavor, delimited, tokens, pos)
  def syncPositionTo(reader: Reader[String]): Unit = this.pos = reader.asInstanceOf[DelimitedReader].pos

  // Pre-scan input looking for given hint label.  Should not change the parser's state (pointer)
  def scanForHint(hintLabel: String): Option[String] = None // invalid for CSV
  def scanForType(path: Path, hintLabel: String, hintModFn: Option[HintValueModifier]): Option[Type] = None // invalid for CSV

  // BufferedIterator controls
  def hasNext: Boolean = pos < tokens.size
  def head: ParseToken[String] = tokens.get(pos)
  def next: ParseToken[String] = {
    val t = tokens.get(pos)
    pos += 1
    t
  }
  def back: ParseToken[String] = {
    if (pos > 0)
      pos -= 1
    else
      pos
    tokens.get(pos)
  }
  def reset(): Unit = pos = 0

  // Print a clip from the input and a grapical pointer to the problem for clarity
  def showError(path: Path, msg: String): String = {
    val errPtr = pos match {
      case p if p >= tokens.size => tokens.get(p - 1).end + 1
      case p                     => tokens.get(p).end
    }
    val (clip, dashes) = errPtr match {
      case ep if ep <= 50 && delimited.length < 80 => (delimited, ep)
      case ep if ep <= 50 => (delimited.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= delimited.length => ("..." + delimited.substring(errPtr - 49), 52)
      case ep => ("..." + delimited.substring(ep - 49, ep + 27) + "...", 52)
    }
    "[" + path.toString + "]: " + msg + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }

  // Read Primitives
  def readBigInt(path: Path): BigInt = BigInt(readString(path))
  def readBoolean(path: Path): Boolean = readString(path).toBoolean
  def readDecimal(path: Path): BigDecimal = BigDecimal(readString(path))
  def readDouble(path: Path): Double = readString(path).toDouble
  def readInt(path: Path): Int = readString(path).toInt
  def readLong(path: Path): Long = readString(path).toLong
  def readString(path: Path): String = {
    tokens.get(pos) match {
      case token if token.tokenType != TokenType.Null =>
        val result = if (token.end < token.begin)
          ""
        else
          token.textValue.replaceAll("\"\"", "\"")
        next
        result
      case _ =>
        next
        // Ugly hackery to trap null here--if we let it go, many non-nullable fields will explode on read,
        // and here we're trying to utilize default values if they are provided by the class
        if (currentField.isDefined) {
          currentField.get.defaultValue.getOrElse(throw new ReadInvalidError("Null or mising fields must either be optional or provide default vales for delimited input")).toString
        } else
          null
    }
  }

  // Read Basic Collections
  def readArray[Elem, To](path: Path, builderFactory: MethodMirror, elementTypeAdapter: TypeAdapter[Elem]): To = {
    val builder = builderFactory().asInstanceOf[Builder[Elem, To]]
    val subReader = jackFlavor.parse(readString(path))
    var i = 0
    while (subReader.head.tokenType != TokenType.End) {
      builder += elementTypeAdapter.read(path \ i, subReader)
      i += 1
    }
    next // consume the end array token
    builder.result
  }

  def readMap[Key, Value, To](path: Path, builderFactory: MethodMirror, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To =
    throw new UOE("Map serialization not available for Delimited encoding")

  def readTuple(path: Path, readFns: List[(Path, Reader[String]) => Any]): List[Any] = {
    val subReader = jackFlavor.parse(readString(path))
    var fnPos = 0
    val tup = readFns.map { fn =>
      fnPos += 1
      fn(path \ fnPos, subReader)
    }
    tup
  }

  // Read fields we know to be object fields (must be in constructor order for Delimited!)
  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead = {
    var fieldCount = 0
    var captured = Map.empty[String, Any] // a place to cache SJCapture'd fields
    val args = fields.values.map { fieldMember =>
      currentField = Some(fieldMember)
      val value = if (fieldMember.valueTypeAdapter.isInstanceOf[classes.CaseClassTypeAdapter[_]] || fieldMember.valueTypeAdapter.isInstanceOf[PlainClassTypeAdapter[_]]) {
        val subReader = jackFlavor.parse(readString(path))
        fieldMember.valueTypeAdapter.read(path, subReader)
      } else
        fieldMember.valueTypeAdapter.read(path, this)
      currentField = None
      value match {
        case null =>
          fieldMember.defaultValue.getOrElse(throw new Exception("BOOM!"))
        case None if fieldMember.defaultValue.isDefined =>
          fieldMember.defaultValue.get
        case _ => value
      }
    }.toArray
    val flags = Array.fill(fields.size)(true)
    ObjectFieldsRead(fieldCount == fields.size, args, flags, captured)
  }

  def skipObject(path: Path): Unit = {} // noop for Delimited
}