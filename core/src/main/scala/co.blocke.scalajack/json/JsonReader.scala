package co.blocke.scalajack
package json

import model._
import model.TokenType._
import util.Path

import scala.util.{ Try, Success, Failure }
import scala.collection.immutable.Map
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

  def lookAheadForField(fieldName: String): Option[String] = {
    if (tokens.get(p).tokenType != BeginObject)
      None
    else {
      var done = tokens.get(p).tokenType == EndObject
      var found: Option[String] = None
      while (p < tokens.size && !done) {
        p += 1
        val jt = tokens.get(p)
        val js = json.substring(jt.begin, jt.end)
        if (js == fieldName) {
          p += 1
          val jt2 = tokens.get(p)
          found = Some(json.substring(jt2.begin, jt2.end))
          done = true
        } else if (tokens.get(p).tokenType == EndObject)
          done = true
      }
      found
    }
  }

  def cloneWithSource(source: String): Transceiver[String] = // used for Any parsing
    new JsonTransciever(source, context, stringTypeAdapter, jackFlavor)

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To = {
    val value = tokens.get(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        var i = 0
        p += 1
        while (p < tokens.size && tokens.get(p).tokenType != EndArray) {
          builder += elementTypeAdapter.read(path \ i, this)
          i += 1
        }
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Array but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readMap[MapKey, MapValue, To](path: Path, canBuildFrom: CanBuildFrom[_, (MapKey, MapValue), To], keyTypeAdapter: TypeAdapter[MapKey], valueTypeAdapter: TypeAdapter[MapValue]): To = {
    val value = tokens.get(p).tokenType match {
      case BeginObject =>
        val builder = canBuildFrom()
        p += 1
        while (p < tokens.size && tokens.get(p).tokenType != EndObject) {
          val key = keyTypeAdapter.read(path \ Path.MapKey, this)
          if (key == null)
            throw new ReadInvalidError(path, "Map keys cannot be null", List.empty[String])
          val value = valueTypeAdapter.read(path \ key.toString, this)
          builder += key -> value
        }
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Map but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readBoolean(path: Path): Boolean = {
    val value = tokens.get(p).tokenType match {
      case True  => true
      case False => false
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Boolean but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
          s"Failed to create BigDecimal value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case Null => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Number (Decimal) but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
          s"Failed to create BigInt value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case Null => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a BigInt but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
          s"Failed to create Double value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Double but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
          s"Failed to create Int value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Int but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
          s"Failed to create Long value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Long but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readObjectFields[T](path: Path, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]]): ObjectFieldResult = { //(Boolean, Array[Any], Array[Boolean]) = {
    val value = tokens.get(p).tokenType match {
      case BeginObject =>
        var fieldCount = 0
        p += 1
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        while (p < tokens.size && tokens.get(p).tokenType != EndObject) {
          val key = json.substring(tokens.get(p).begin, tokens.get(p).end)
          p += 1
          fields.get(key) match {
            case Some(oneField) =>
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ key, this)
              flags(oneField.index) = true
              fieldCount += 1
            case _ =>
          }
        }
        ObjectFieldResult(fieldCount == fields.size, args, flags)
      case Null =>
        null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Object (map with String keys) but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
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
        throw new ReadUnexpectedError(path, s"Expected a String but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readTuple(path: Path, readFns: List[(Path, Transceiver[String]) => Any]): List[Any] = {
    var fnPos = 0
    val value = tokens.get(p).tokenType match {
      case BeginArray =>
        p += 1
        val tup = readFns.map { fn =>
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
          throw new ReadUnexpectedError(path, s"Too many values in tuple list.  Should be ${readFns.size} but actually there's ${readFns.size + c}", List("EndArray"))
        }
        tup
      case Null =>
        null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Tuple (Array) but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }
}
