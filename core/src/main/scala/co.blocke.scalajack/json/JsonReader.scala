package co.blocke.scalajack
package json

import model._
import model.TokenType._
import util.Path

import scala.util.{ Try, Success, Failure }
import scala.collection.immutable.{ ListMap, Map }
import scala.collection.generic.CanBuildFrom
import java.util.ArrayList

trait JsonReader extends Reader[String] {

  this: JsonTransciever =>

  val context: Context

  val json: String

  private var p: Int = 0
  private var saved: Int = -1

  lazy val tokens = tokenizer.tokenize(json).asInstanceOf[ArrayList[JsonToken]] //ArrayBuffer[JsonToken]]

  def savePos() = saved = p

  def rollbackToSave() = p = saved

  def peek(): TokenType = tokens.get(p).tokenType

  def lastTokenText(): String = {
    val jt = tokens.get(p - 1)
    json.substring(jt.begin, jt.end)
  }

  def skip() = if (p < tokens.size()) p += 1

  def isDone(): Boolean = p == tokens.size - 1
  //  def show(): String = "Tokens: " + tokens.size + "  P: " + p

  // WARNING: Presumes we're in a JSON object!
  def lookAheadForTypeHint(fieldName: String, typeMaterializer: String => Type): Option[Type] = {
    savePos()
    var objStack = 0
    var arrayStack = 0
    p += 1
    var done = tokens.get(p).tokenType == EndObject
    var found: Option[Type] = None
    while (p < tokens.size && !done) {
      tokens.get(p).tokenType match {
        case TokenType.String if (objStack == 0 && arrayStack == 0) =>
          val jt = tokens.get(p)
          val js = json.substring(jt.begin, jt.end)
          if (js == fieldName) {
            p += 2
            done = true
            tokens.get(p).tokenType match {
              case TokenType.String =>
                val jt2 = tokens.get(p)
                val hintString = json.substring(jt2.begin, jt2.end)
                found = Try(typeMaterializer(hintString)).toOption
              case _ => p -= 1 // do nothing
            }
          } else if (tokens.get(p + 2).tokenType == TokenType.String)
            p += 2 // skip colon + string value if not what we're looking for
          else
            p += 1 // skip colon
        case TokenType.BeginObject =>
          objStack += 1
        case TokenType.BeginArray =>
          arrayStack += 1
        case TokenType.EndArray =>
          arrayStack -= 1
        case TokenType.EndObject =>
          if (objStack == 0) {
            p -= 1
            done = true
          } else
            objStack -= 1
        case _ =>
      }
      p += 1
    }
    if (found.isDefined)
      rollbackToSave()
    found
  }

  def cloneWithSource(source: String): Transceiver[String] = // used for Any parsing
    new JsonTransciever(source, context, stringTypeAdapter, jackFlavor)

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To = {
    val value = tokens.get(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        var i = 0
        p += 1
        var first = true
        while (p < tokens.size && tokens.get(p).tokenType != EndArray) {
          if (!first) {
            if (tokens.get(p).tokenType != Comma)
              throw new ReadUnexpectedError(path \ i, "Expected comma here.\n" + showError(), List("Comma"))
            p += 1
          }
          first = false
          builder += elementTypeAdapter.read(path \ i, this)
          i += 1
        }
        if (tokens.get(p).tokenType != EndArray)
          throw new ReadUnexpectedError(path, s"Unterminated JSON array\n" + showError())
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Array but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readMap[MapKey, MapValue, To](path: Path, canBuildFrom: CanBuildFrom[_, (MapKey, MapValue), To], keyTypeAdapter: TypeAdapter[MapKey], valueTypeAdapter: TypeAdapter[MapValue]): To = {
    val value = tokens.get(p).tokenType match {
      case BeginObject =>
        val builder = canBuildFrom()
        p += 1
        var first = true
        while (p < tokens.size && tokens.get(p).tokenType != EndObject) {
          if (!first) {
            if (tokens.get(p).tokenType != Comma)
              throw new ReadUnexpectedError(path, "Expected comma here.\n" + showError(), List("Comma"))
            p += 1
          }
          first = false
          val key = keyTypeAdapter.read(path \ Path.MapKey, this)
          if (key == null)
            throw new ReadInvalidError(path, "Map keys cannot be null\n" + showError(), List.empty[String])
          if (tokens.get(p).tokenType != TokenType.Colon)
            throw new ReadUnexpectedError(path \ key.toString, s"Expected a colon here", List("Colon"))
          p += 1
          val value = valueTypeAdapter.read(path \ key.toString, this)
          builder += key -> value
        }
        if (tokens.get(p).tokenType != EndObject)
          throw new ReadUnexpectedError(path, s"Unterminated JSON object\n" + showError())
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Map but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readBoolean(path: Path): Boolean = {
    val value = tokens.get(p).tokenType match {
      case True  => true
      case False => false
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Boolean but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readDecimal(path: Path): BigDecimal = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(BigDecimal(json.substring(jt.begin, jt.end))) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create BigDecimal value from parsed text ${json.substring(jt.begin, jt.end)}\n" + showError(),
          List.empty[String], u)
      }
      case Null => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Number (Decimal) but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readBigInt(path: Path): BigInt = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(BigInt(json.substring(jt.begin, jt.end))) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create BigInt value from parsed text ${json.substring(jt.begin, jt.end)}\n" + showError(),
          List.empty[String], u)
      }
      case Null => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a BigInt but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readDouble(path: Path): Double = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toDouble) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Double value from parsed text ${json.substring(jt.begin, jt.end)}\n" + showError(),
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Double but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readInt(path: Path): Int = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toInt) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Int value from parsed text ${json.substring(jt.begin, jt.end)}\n" + showError(),
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Int but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readLong(path: Path): Long = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toLong) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Long value from parsed text ${json.substring(jt.begin, jt.end)}\n" + showError(),
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Long but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readObjectFields[T](path: Path, isSJCapture: Boolean, fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldResult = {
    val value = tokens.get(p).tokenType match {
      case BeginObject =>
        var fieldCount = 0
        var captured = if (isSJCapture) Map.empty[String, Any] else null
        p += 1
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        var first = true
        while (p < tokens.size && tokens.get(p).tokenType != EndObject) {
          if (!first) {
            if (tokens.get(p).tokenType != Comma)
              throw new ReadUnexpectedError(path, "Expected comma here.\n" + showError(), List("Comma"))
            p += 1
          }
          first = false
          if (tokens.get(p).tokenType != TokenType.String)
            throw new ReadUnexpectedError(path, s"Expected a JSON string here\n" + showError(), List("String"))
          val key = json.substring(tokens.get(p).begin, tokens.get(p).end)
          p += 1
          if (tokens.get(p).tokenType != TokenType.Colon)
            throw new ReadUnexpectedError(path \ key, s"Expected a colon here\n" + showError(), List("Colon"))
          p += 1
          fields.get(key) match {
            case Some(oneField) =>
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ key, this)
              flags(oneField.index) = true
              fieldCount += 1
            case _ if (isSJCapture) =>
              captured = captured.+((key, context.typeAdapterOf[Any].read(path \ key, this)))
            case _ =>
              // Skip over field not in class if we're not capturing
              context.typeAdapterOf[Any].read(path \ key, this)
          }
        }
        if (tokens.get(p).tokenType != EndObject)
          throw new ReadUnexpectedError(path, s"Unterminated JSON object\n" + showError())
        ObjectFieldResult(fieldCount == fields.size, args, flags, Option(captured))
      case Null =>
        null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Object (map with String keys) but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readString(path: Path): String = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case String =>
        var builder: StringBuilder = null
        var startOfUnescapedCharacters = jt.begin
        var position = jt.begin

        while (position < jt.end) {
          json(position) match {
            case '\\' =>
              if (builder == null) builder = new StringBuilder()
              builder.appendAll(json.toCharArray, startOfUnescapedCharacters, position - startOfUnescapedCharacters)

              json(position + 1) match {
                case '"' =>
                  builder.append('"')
                  position += 2

                case '\\' =>
                  builder.append('\\')
                  position += 2

                case '/' =>
                  builder.append('/')
                  position += 2

                case 'b' =>
                  builder.append('\b')
                  position += 2

                case 'f' =>
                  builder.append('\f')
                  position += 2

                case 'n' =>
                  builder.append('\n')
                  position += 2

                case 'r' =>
                  builder.append('\r')
                  position += 2

                case 't' =>
                  builder.append('\t')
                  position += 2

                case 'u' =>
                  val hexEncoded = json.substring(position + 2, position + 6)
                  val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
                  builder.append(unicodeChar)
                  position += 6
              }

              startOfUnescapedCharacters = position

            case ch =>
              position += 1
          }
        }

        if (builder == null) {
          json.substring(jt.begin, jt.end)
        } else {
          builder.appendAll(json.toCharArray, startOfUnescapedCharacters, jt.end - startOfUnescapedCharacters)
          builder.toString()
        }
      case Null => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a String but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readTuple(path: Path, readFns: List[(Path, Transceiver[String]) => Any]): List[Any] = {
    var fnPos = 0
    val value = tokens.get(p).tokenType match {
      case BeginArray =>
        p += 1
        var first = true
        val tup = readFns.map { fn =>
          if (!first) {
            if (tokens.get(p).tokenType != Comma) {
              throw new ReadUnexpectedError(path, "Expected comma here.\n" + showError())
            }
            p += 1
          }
          first = false
          val a = fn(path \ fnPos, this)
          fnPos += 1
          a
        }
        if (tokens.get(p).tokenType != EndArray) {
          var c = 0
          while (tokens.get(p).tokenType != EndArray) {
            c += 1
            p += 1
          }
          throw new ReadUnexpectedError(path, s"Too many values in tuple list.  Should be ${readFns.size} but actually there's ${readFns.size + c}\n" + showError(), List("EndArray"))
        }
        tup
      case Null =>
        null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Tuple (Array) but parsed ${tokens.get(p).tokenType}\n" + showError(), List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  override def showError(): String = {
    if (p >= tokens.size)
      p = tokens.size - 1
    val charPos = tokens.get(p).begin
    val startPosOffset = if (charPos - 50 < 0) charPos else 50
    val startPos = charPos - startPosOffset
    val endPos = if (charPos + 50 > json.length) json.length else charPos + 50
    val buf = new StringBuffer()
    buf.append(json.subSequence(startPos, endPos).toString + "\n")
    val line = json.subSequence(startPos, startPos + startPosOffset).toString.map(_ match {
      case '\n' => '\n'
      case _    => '-'
    }).mkString + "^"
    buf.append(line)
    // buf.append("-" * startPosOffset + "^")
    buf.toString
  }
}
