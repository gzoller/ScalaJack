package co.blocke.scalajack
package json

import java.util.ArrayList

import model._
import util.{ Path, StringBuilder }

import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scala.collection.immutable.{ ListMap, Map }

case class JsonReader(jackFlavor: JackFlavor[String], json: String, tokens: ArrayList[JsonToken], initialPos: Int = 0) extends Reader[String] {

  private var pos = initialPos

  @inline private def expect[T](t: TokenType.Value, path: Path, fn: (ParseToken[String]) => T, isNullable: Boolean = false): T =
    if (hasNext) {
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
    } else
      throw new ReadUnexpectedError(showError(path, "Premature end of input.  Expected " + t + " here"))

  @inline private def assertExists[T](t: TokenType.Value, path: Path): Unit =
    if (hasNext) {
      if (head.tokenType == t)
        next
      else
        throw new ReadUnexpectedError(showError(path, s"Expected $t here but found ${head.tokenType}"))
    } else
      throw new ReadUnexpectedError(showError(path, "Premature end of input.  Expected " + t + " here"))

  def copy: Reader[String] = JsonReader(jackFlavor, json, tokens, pos)
  def syncPositionTo(reader: Reader[String]) = this.pos = reader.asInstanceOf[JsonReader].pos

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
  def abort() = pos = tokens.size

  /**
   * Nondestructive (doesn't change pointer position) lookahead for a named field (presumes an object)
   * @param hintLabel Name of field to search for
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
          p += 2
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
            throw new ReadInvalidError(showError(path, s"Failed to apply type modifier to type member hint ${hintValue}"))
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

  def readBigInt(path: Path): BigInt = expect(TokenType.Number, path, (pt: ParseToken[String]) => BigInt(pt.textValue), true)
  def readBoolean(path: Path): Boolean = expect(TokenType.Boolean, path, (pt: ParseToken[String]) => pt.textValue.toBoolean)
  def readDecimal(path: Path): BigDecimal = expect(TokenType.Number, path, (pt: ParseToken[String]) => BigDecimal(pt.textValue), true)
  def readDouble(path: Path): Double = expect(TokenType.Number, path, (pt: ParseToken[String]) => pt.textValue.toDouble)
  def readInt(path: Path): Int = expect(TokenType.Number, path, (pt: ParseToken[String]) => pt.textValue.toInt)
  def readLong(path: Path): Long = expect(TokenType.Number, path, (pt: ParseToken[String]) => pt.textValue.toLong)
  def readString(path: Path): String = expect(TokenType.String, path, _readString, true)

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To =
    expect(TokenType.BeginArray, path, (pt: ParseToken[String]) => "", true) match {
      case "" =>
        val builder = canBuildFrom()
        var first = true
        var i = 0
        while (head.tokenType != TokenType.EndArray) {
          if (first)
            first = false
          else
            assertExists(TokenType.Comma, path \ i)
          builder += elementTypeAdapter.read(path \ i, this)
          i += 1
        }
        next // consume the end array token
        builder.result
      case null => null.asInstanceOf[To]
    }

  def readMap[MapKey, MapValue, To](path: Path, canBuildFrom: CanBuildFrom[_, (MapKey, MapValue), To], keyTypeAdapter: TypeAdapter[MapKey], valueTypeAdapter: TypeAdapter[MapValue]): To =
    expect(TokenType.BeginObject, path, (pt: ParseToken[String]) => "", true) match {
      case "" =>
        val builder = canBuildFrom()
        var first = true
        while (head.tokenType != TokenType.EndObject) {
          if (first)
            first = false
          else
            assertExists(TokenType.Comma, path)
          keyTypeAdapter.read(path \ Path.MapKey, this) match {
            case null =>
              throw new ReadInvalidError(showError(path, "Map keys cannot be null"))
            case key =>
              assertExists(TokenType.Colon, path \ key.toString)
              builder += key -> valueTypeAdapter.read(path \ key.toString, this)
          }
        }
        next // consume EndObject
        builder.result
      case null => null.asInstanceOf[To]
    }

  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldsRead =
    expect(TokenType.BeginObject, path, (pt: ParseToken[String]) => "", true) match {
      case "" =>
        var fieldCount = 0
        var captured = Map.empty[String, Any] // a place to cache SJCapture'd fields
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        var first = true
        while (head.tokenType != TokenType.EndObject) {
          if (first)
            first = false
          else
            assertExists(TokenType.Comma, path)
          val fieldName = expect(TokenType.String, path, (pt: ParseToken[String]) => pt.textValue, false)
          assertExists(TokenType.Colon, path \ fieldName)
          fields.get(fieldName) match {
            case Some(oneField) =>
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ fieldName, this)
              flags(oneField.index) = true
              fieldCount += 1
            case _ if (isSJCapture) =>
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
  def skipObject(path: Path) =
    if (head.tokenType == TokenType.BeginObject)
      readMap[String, Any, Map[String, Any]](path, Map.canBuildFrom[String, Any], jackFlavor.stringTypeAdapter, jackFlavor.anyTypeAdapter)

  def readTuple(path: Path, readFns: List[(Path, Reader[String]) => Any]): List[Any] =
    expect(TokenType.BeginArray, path, (pt: ParseToken[String]) => "", true) match {
      case "" =>
        var fnPos = -1
        var first = true
        val tup = readFns.map { fn =>
          if (first)
            first = false
          else
            assertExists(TokenType.Comma, path)
          fnPos += 1
          fn(path \ fnPos, this)
        }
        assertExists(TokenType.EndArray, path)
        tup
      case null => null
    }

  def showError(path: Path, msg: String): String = {
    val errPtr = pos match {
      case p if p >= tokens.size && tokens.size <= 1 => 0
      case p if p >= tokens.size                     => tokens.get(p - 1).end + 1
      case p                                         => tokens.get(p).end
    }
    val (clip, dashes) = errPtr match {
      case ep if ep <= 50 && json.length < 80      => (json, ep)
      case ep if ep <= 50                          => (json.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= json.length => ("..." + json.substring(errPtr - 49), 52)
      case ep                                      => ("..." + json.substring(ep - 49, ep + 27) + "...", 52)
    }
    "[" + path.toString + "]: " + msg + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }

  private val _readString = (pt: ParseToken[String]) => {
    var builder: StringBuilder = null
    var startOfUnescapedCharacters = pt.asInstanceOf[JsonToken].begin
    var position = startOfUnescapedCharacters
    val end = pt.asInstanceOf[JsonToken].end + 1

    while (position < end) {
      json(position) match {
        case '\\' =>
          if (builder == null)
            builder = new StringBuilder()
          builder += json.substring(startOfUnescapedCharacters, position)

          json(position + 1) match {
            case '"' =>
              builder += "\""
              position += 2

            case '\\' =>
              builder += "\\"
              position += 2

            case 'b' =>
              builder += "\b"
              position += 2

            case 'f' =>
              builder += "\f"
              position += 2

            case 'n' =>
              builder += "\n"
              position += 2

            case 'r' =>
              builder += "\r"
              position += 2

            case 't' =>
              builder += "\t"
              position += 2

            case 'u' =>
              val hexEncoded = json.substring(position + 2, position + 6)
              val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
              builder += unicodeChar.toString
              position += 6
          }
          startOfUnescapedCharacters = position

        case _ =>
          position += 1
      }
    }

    if (builder == null) {
      pt.textValue
    } else {
      builder += json.substring(startOfUnescapedCharacters, end)
      builder.result()
    }
  }
}
