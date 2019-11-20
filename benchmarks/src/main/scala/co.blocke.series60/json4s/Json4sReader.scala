package co.blocke.series60
package json4s

import model._
import typeadapter.CanBuildMapTypeAdapter
import util.Path
import typeadapter.TupleTypeAdapterFactory

import org.json4s._
import scala.util.Try
import scala.collection.immutable.{ ListMap, Map }
import scala.collection.mutable.Builder

case class Json4sReader(jackFlavor: JackFlavor[JValue], json: JValue, tokens: java.util.ArrayList[JValueToken], initialPos: Int = 0) extends model.Reader[JValue] {

  private var pos = initialPos

  // For skipping objects
  private lazy val mapAnyTypeAdapter: TypeAdapter[Map[Any, Any]] = jackFlavor.context.typeAdapterOf[Map[Any, Any]]

  @inline private def expect[T](t: TokenType.Value, path: Path, fn: ParseToken[JValue] => T, isNullable: Boolean = false): T =
    next match {
      case tok if tok.tokenType == t =>
        Try(fn(tok)).getOrElse {
          back
          throw new ReadMalformedError(showError(path, s"Unable to read value (e.g. bad number format)"))
        }
      case tok if tok.tokenType == TokenType.Null && isNullable =>
        null.asInstanceOf[T]
      case tok =>
        back
        throw new ReadUnexpectedError(showError(path, "Expected " + t + s" here but found " + tok.tokenType), tok.tokenType == TokenType.Null)
    }

  @inline private def assertExists[T](t: TokenType.Value, path: Path): Unit =
    if (head.tokenType == t)
      next
    else
      throw new ReadUnexpectedError(showError(path, s"Expected $t here but found ${head.tokenType}"))

  def copy: model.Reader[JValue] = Json4sReader(jackFlavor, json, tokens, pos)
  def syncPositionTo(reader: model.Reader[JValue]): Unit = this.pos = reader.asInstanceOf[Json4sReader].pos

  def hasNext: Boolean = pos < tokens.size
  def head: ParseToken[JValue] = tokens.get(pos)
  def next: ParseToken[JValue] = {
    val t = tokens.get(pos)
    pos += 1
    t
  }
  def back: ParseToken[JValue] = {
    if (pos > 0)
      pos -= 1
    tokens.get(pos)
  }
  def reset(): Unit = pos = 0

  /**
   * Nondestructive (doesn't change pointer position) lookahead for a named field (presumes an object)
   * @param label Name of field to search for
   * @return value of the found field
   */
  private def _scanForString(label: String): (Option[String], Int) = {
    var level = 0 // we only care about looking for hints at level 1 (presume first token is '{')
    var p = pos
    var found: Option[String] = None
    while (p < tokens.size && found.isEmpty) {
      tokens.get(p) match {
        case tok if tok.tokenType == TokenType.String && level == 1 =>
          val value = tok.textValue
          p += 1
          if (value == label) {
            if (tokens.get(p).tokenType == TokenType.String) {
              found = Some(tokens.get(p).textValue)
            } else
              p = tokens.size
          }
        case tok if tok.tokenType == TokenType.BeginObject || tok.tokenType == TokenType.BeginArray =>
          level += 1
        case tok if tok.tokenType == TokenType.EndArray || tok.tokenType == TokenType.EndObject =>
          level -= 1
        case _ =>
      }
      p += 1
    }
    (found, p)
  }

  def scanForType(path: Path, hintLabel: String, hintModFn: Option[HintValueModifier]): Option[Type] = {
    val (found, p) = _scanForString(hintLabel)
    found match {
      case Some(hintValue) if hintModFn.isDefined =>
        try {
          Some(hintModFn.get.apply(hintValue))
        } catch {
          case _: Throwable =>
            pos = p - 1
            throw new ReadInvalidError(showError(path, s"Failed to apply type modifier to type member hint $hintValue"))
        }
      case Some(hintValue) =>
        val savedP = pos
        pos = p - 1
        val result = Some(jackFlavor.typeTypeAdapter.typeNameToType(path, hintValue, this))
        pos = savedP
        result
      case None =>
        None
    }
  }
  def scanForHint(hintLabel: String): Option[String] = _scanForString(hintLabel)._1

  def readBigInt(path: Path): BigInt = expect(TokenType.Number, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JInt].values, true)
  def readBoolean(path: Path): Boolean = expect(TokenType.Boolean, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JBool].value)
  def readDecimal(path: Path): BigDecimal = expect(TokenType.Number, path, (pt: ParseToken[JValue]) => pt.input match {
    case v: JInt     => BigDecimal(v.num)
    case v: JDecimal => v.num
    case v: JLong    => BigDecimal(v.num)
    case v: JDouble  => BigDecimal(v.num)
    // $COVERAGE-OFF$Can't happen--here to silence compiler warnings of match incompleness
    case _           => null
    // $COVERAGE-ON$
  }, true)
  def readDouble(path: Path): Double = expect(TokenType.Number, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JDouble].num)
  def readInt(path: Path): Int = expect(TokenType.Number, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JInt].values.intValue)
  def readLong(path: Path): Long = expect(TokenType.Number, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JLong].values.longValue())
  def readString(path: Path): String = expect(TokenType.String, path, (pt: ParseToken[JValue]) => pt.input.asInstanceOf[JString].s, true)

  def readArray[Elem, To](path: Path, builderFactory: MethodMirror, elementTypeAdapter: TypeAdapter[Elem]): To =
    expect(TokenType.BeginArray, path, (pt: ParseToken[JValue]) => "", true) match {
      case "" =>
        val builder = builderFactory().asInstanceOf[collection.mutable.Builder[Elem, To]]
        var i = 0
        while (head.tokenType != TokenType.EndArray) {
          builder += elementTypeAdapter.read(path \ i, this)
          i += 1
        }
        next // consume the end array token
        builder.result
      case null => null.asInstanceOf[To]
    }

  def readMap[Key, Value, To](path: Path, builderFactory: MethodMirror, keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value]): To =
    expect(TokenType.BeginObject, path, (_) => "", true) match {
      case "" =>
        val builder = builderFactory().asInstanceOf[Builder[(Key, Value), To]]
        while (head.tokenType != TokenType.EndObject) {
          keyTypeAdapter.read(path \ Path.MapKey, this, true) match {
            case null =>
              // $COVERAGE-OFF$Shouldn't be possible in JValue.  Left here in case
              throw new ReadInvalidError(showError(path, "Map keys cannot be null"))
            // $COVERAGE-ON$
            case key =>
              builder += key -> valueTypeAdapter.read(path \ key.toString, this)
          }
        }
        next // consume EndObject
        builder.result
      case null => null.asInstanceOf[To]
    }

  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead =
    expect(TokenType.BeginObject, path, (pt: ParseToken[JValue]) => "", isNullable = true) match {
      case "" =>
        var fieldCount = 0
        var captured = Map.empty[String, Any] // a place to cache SJCapture'd fields
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        while (head.tokenType != TokenType.EndObject) {
          val fieldName = expect(TokenType.String, path, (pt: ParseToken[JValue]) => pt.textValue, false)
          fields.get(fieldName) match {
            case Some(oneField) =>
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ fieldName, this)
              flags(oneField.index) = true
              fieldCount += 1
            case _ if isSJCapture =>
              captured = captured.+((fieldName, jackFlavor.anyTypeAdapter.asInstanceOf[typeadapter.AnyTypeAdapter]._read(path \ fieldName, this, true)))
            case _ =>
              // Skip over field not in class if we're not capturing
              jackFlavor.anyTypeAdapter.read(path \ fieldName, this)
          }
        }
        next // consume EndObject
        ObjectFieldsRead(fieldCount == fields.size, args, flags, captured)
      case null => null
    }

  // Skip object by reading it as a Map and ignoring the result
  def skipObject(path: Path): Unit =
    if (head.tokenType == TokenType.BeginObject)
      readMap[String, Any, Map[String, Any]](path, mapAnyTypeAdapter.asInstanceOf[CanBuildMapTypeAdapter[Any, Any, Map[Any, Any]]].builderFactory, jackFlavor.stringTypeAdapter, jackFlavor.anyTypeAdapter)

  def readTuple(path: Path, readFns: List[TupleTypeAdapterFactory.TupleField[_]]): List[Any] =
    expect(TokenType.BeginArray, path, (pt: ParseToken[JValue]) => "", true) match {
      case "" =>
        var fnPos = -1
        val tup = readFns.map { fn =>
          fnPos += 1
          fn.read(path \ fnPos, this)
        }
        assertExists(TokenType.EndArray, path)
        tup
    }

  def showError(path: Path, msg: String): String =
    "[" + path.toString + "]: " + msg
}
