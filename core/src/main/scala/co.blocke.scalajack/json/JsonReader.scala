package co.blocke.scalajack
package json

import model._
import model.TokenType._
import util.Path

import scala.util.{ Try, Success, Failure }
import scala.collection.immutable.Map
import scala.collection.generic.CanBuildFrom
import java.util.ArrayList
import org.apache.commons.text.StringEscapeUtils.unescapeJson

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
  def tokenText(): String = {
    val jt = tokens.get(p)
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
          found = Some(tokenText)
          done = true
        } else if (tokens.get(p).tokenType == EndObject)
          done = true
      }
      found
    }
  }

  def cloneWithSource(source: String): Transceiver[String] = // used for Any parsing
    new JsonTransciever(source, context)

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To =
    tokens.get(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        var i = 0
        p += 1
        while (p < tokens.size && tokens.get(p).tokenType != EndArray) {
          builder += elementTypeAdapter.read(path \ i, this, isMapKey)
          i += 1
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens.get(p)
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readArray(path, canBuildFrom, elementTypeAdapter, false)
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Array but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }

  def readMap[Key, Value, To](path: Path, canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To =
    tokens.get(p).tokenType match {
      case BeginObject =>
        val builder = canBuildFrom()
        p += 1
        while (p < tokens.size && tokens.get(p).tokenType != EndObject) {
          val key = keyTypeAdapter.read(path \ "(map key)", this, true)
          val value = valueTypeAdapter.read(path \ key.toString, this, false)
          builder += key -> value
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens.get(p)
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readMap(path, canBuildFrom, keyTypeAdapter, valueTypeAdapter, false)
      case _ =>
        throw new ReadUnexpectedError(path, "Expected a Map but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }

  def readBoolean(path: Path, isMapKey: Boolean): Boolean = {
    val value = tokens.get(p).tokenType match {
      case True  => true
      case False => false
      case String if isMapKey =>
        val jt = tokens.get(p)
        val s = json.substring(jt.begin, jt.end)
        if (s == "true") true else false
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a Boolean but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }

  def readDecimal(path: Path, isMapKey: Boolean): BigDecimal = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(BigDecimal(json.substring(jt.begin, jt.end))) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create BigDecimal value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case String if isMapKey => Try(BigDecimal(json.substring(jt.begin, jt.end))) match {
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

  def readBigInt(path: Path, isMapKey: Boolean): BigInt = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(BigInt(json.substring(jt.begin, jt.end))) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create BigInt value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case String if isMapKey => Try(BigInt(json.substring(jt.begin, jt.end))) match {
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

  def readDouble(path: Path, isMapKey: Boolean): Double = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toDouble) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Double value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case String if isMapKey => Try(json.substring(jt.begin, jt.end).toDouble) match {
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

  def readInt(path: Path, isMapKey: Boolean): Int = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toInt) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Int value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case String if isMapKey => Try(json.substring(jt.begin, jt.end).toInt) match {
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

  def readLong(path: Path, isMapKey: Boolean): Long = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case Number => Try(json.substring(jt.begin, jt.end).toLong) match {
        case Success(u) => u
        case Failure(u) => throw new ReadMalformedError(
          path,
          s"Failed to create Long value from parsed text ${json.substring(jt.begin, jt.end)}",
          List.empty[String], u)
      }
      case String if isMapKey => Try(json.substring(jt.begin, jt.end).toLong) match {
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

  def readObjectFields[T](path: Path, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): ObjectFieldResult = { //(Boolean, Array[Any], Array[Boolean]) = {
    tokens.get(p).tokenType match {
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
              args(oneField.index) = oneField.valueTypeAdapter.read(path \ key, this, false)
              flags(oneField.index) = true
              fieldCount += 1
            case _ =>
          }
        }
        p += 1
        ObjectFieldResult(fieldCount == fields.size, args, flags)
      case Null =>
        null
      case String if isMapKey =>
        val jt = tokens.get(p)
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readObjectFields(path, fields, isMapKey)
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected an Object (map with String keys) but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
  }

  def readString(path: Path): String = {
    val jt = tokens.get(p)
    val value = jt.tokenType match {
      case String => json.substring(jt.begin, jt.end)
      //      case String => unescapeJson(json.substring(jt.begin, jt.end))
      case Null   => null
      case _ =>
        throw new ReadUnexpectedError(path, s"Expected a String but parsed ${tokens.get(p).tokenType}", List(tokens.get(p).tokenType.toString))
    }
    p += 1
    value
  }
}
