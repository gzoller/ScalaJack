package co.blocke.scalajack
package delimited

import model._
import util.Path
import java.util.ArrayList
import java.lang.{ UnsupportedOperationException => UOE }

import typeadapter.TupleTypeAdapterFactory

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }
import scala.collection.immutable.{ ListMap, Map }

case class DelimitedReader(jackFlavor: JackFlavor[String], delimited: String, tokens: ArrayList[DelimitedToken], initialPos: Int = 0) extends Reader[String] {

  private var pos = initialPos

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
      case token if token.tokenType == TokenType.Null =>
        next
        null
      case token =>
        next
        token.textValue
    }
  }

  // Read Basic Collections
  def readArray[Elem, To](path: Path, builderFactory: MethodMirror, elementTypeAdapter: TypeAdapter[Elem]): To =
    if (head.tokenType == TokenType.Null) {
      next
      null.asInstanceOf[To]
    } else {
      val builder = builderFactory().asInstanceOf[Builder[Elem, To]]
      var i = 0
      while (head.tokenType != TokenType.End) {
        val tryValue = if (head.tokenType == TokenType.QuotedString && (elementTypeAdapter.isInstanceOf[Collectionish] || elementTypeAdapter.isInstanceOf[Classish]))
          Try(elementTypeAdapter.read(path \ i, jackFlavor.parse(readString(path))))
        else
          Try(elementTypeAdapter.read(path \ i, this))
        tryValue match {
          case Success(x) => builder += x
          case Failure(x) =>
            back
            throw new ReadMalformedError(showError(path, x.getMessage()))
        }
        i += 1
      }
      builder.result
    }

  def readMap[Key, Value, To](path: Path, builderFactory: MethodMirror, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To =
    throw new UOE("Map serialization not available for Delimited encoding")

  def readTuple(path: Path, readFns: List[TupleTypeAdapterFactory.TupleField[_]]): List[Any] = {
    var fnPos = 0
    val tup = readFns.map { fn =>
      fnPos += 1
      if (head.tokenType == TokenType.QuotedString && (fn.valueTypeAdapter.isInstanceOf[Collectionish] || fn.valueTypeAdapter.isInstanceOf[Classish])) {
        fn.read(path \ fnPos, jackFlavor.parse(readString(path)))
      } else {
        fn.read(path \ fnPos, this)
      }
    }
    tup
  }

  // Read fields we know to be object fields (must be in constructor order for Delimited!)
  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead = {
    var fieldCount = 0
    val captured = Map.empty[String, Any] // a place to cache SJCapture'd fields
    val args = fields.values.map { fieldMember =>
      val tryValue = head match {
        case tok if tok.tokenType == TokenType.Null =>
          next
          Success(null)
        case tok if (tok.tokenType == TokenType.QuotedString && (fieldMember.valueTypeAdapter.isInstanceOf[Collectionish] || fieldMember.valueTypeAdapter.isInstanceOf[Classish])) =>
          Try(fieldMember.valueTypeAdapter.read(path \ fieldCount, jackFlavor.parse(readString(path))))
        case _ =>
          Try(fieldMember.valueTypeAdapter.read(path \ fieldCount, this))
      }
      val value = tryValue match {
        case Success(v) => v
        case Failure(x) if (x.isInstanceOf[SJError]) =>
          throw x
        case Failure(x) =>
          back
          throw new ReadMalformedError(showError(path \ fieldCount, x.getMessage()))
      }
      fieldCount += 1
      value match {
        case null =>
          fieldMember.defaultValue.getOrElse {
            back
            throw new ReadInvalidError(showError(path, "Null or mising fields must either be optional or provide default vales for delimited input"))
          }
        case None if fieldMember.defaultValue.isDefined =>
          fieldMember.defaultValue.get
        case _ => value
      }
    }.toArray
    val flags = Array.fill(fields.size)(true)
    ObjectFieldsRead(true, args, flags, captured)
  }

  def skipObject(path: Path): Unit = {} // noop for Delimited
}