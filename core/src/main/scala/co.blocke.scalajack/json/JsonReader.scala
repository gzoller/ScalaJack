package co.blocke.scalajack
package json

import model._
import model.TokenType._
import util.Path

import scala.collection.immutable.Map
import scala.collection.generic.CanBuildFrom

case class JsonReader(json: String, tokenizer: Tokenizer[String] = JsonTokenizer()) extends Reader {

  type WIRE = String

  private var p: Int = 0
  private var saved: Int = -1

  val tokens = tokenizer.tokenize(json)

  def savePos() = saved = p
  def rollbackToSave() = p = saved
  def peek(): TokenType = tokens(p).tokenType
  def tokenText(): String = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    json.substring(jt.begin, jt.end)
  }
  def skip() = if (p < tokens.length) p += 1
  def lookAheadForField(fieldName: String): Option[String] = {
    if (tokens(p).tokenType != BeginObject)
      None
    else {
      var done = tokens(p).tokenType == EndObject
      var found: Option[String] = None
      while (p < tokens.length && !done) {
        p += 1
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        if (js == fieldName) {
          p += 1
          found = Some(tokenText)
          done = true
        } else if (tokens(p).tokenType == EndObject)
          done = true
      }
      found
    }
  }

  def cloneWithSource(source: String): Reader = // used for Any parsing
    new JsonReader(source, tokenizer)

  def readArray[Elem, To](path: Path, canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To =
    tokens(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        var i = 0
        p += 1
        while (p < tokens.length && tokens(p).tokenType != EndArray) {
          builder += elementTypeAdapter.read(path \ i, this, isMapKey)
          i += 1
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readArray(path, canBuildFrom, elementTypeAdapter, false)
      case _ =>
        throw new Exception("Boom -- expected an Array but got " + tokens(p).tokenType)
    }

  def readMap[Key, Value, To](path: Path, canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To =
    tokens(p).tokenType match {
      case BeginObject =>
        val builder = canBuildFrom()
        p += 1
        while (p < tokens.length && tokens(p).tokenType != EndObject) {
          val key = keyTypeAdapter.read(path \ "(map key)", this, true)
          val value = valueTypeAdapter.read(path \ key.toString, this, false)
          builder += key -> value
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readMap(path, canBuildFrom, keyTypeAdapter, valueTypeAdapter, false)
      case _ =>
        throw new Exception("Boom -- expected an Object but got " + tokens(p).tokenType)
    }

  def readBoolean(isMapKey: Boolean): Boolean = {
    val value = tokens(p).tokenType match {
      case True  => true
      case False => false
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val s = json.substring(jt.begin, jt.end)
        if (s == "true") true else false
      case x => throw new Exception("Boom -- expected a Boolean but got " + x)
    }
    p += 1
    value
  }

  def readDecimal(isMapKey: Boolean): BigDecimal = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number             => BigDecimal(json.substring(jt.begin, jt.end))
      case String if isMapKey => BigDecimal(json.substring(jt.begin, jt.end))
      case Null               => null
      case x                  => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readBigInt(isMapKey: Boolean): BigInt = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number             => BigInt(json.substring(jt.begin, jt.end))
      case String if isMapKey => BigInt(json.substring(jt.begin, jt.end))
      case Null               => null
      case x                  => throw new Exception("Boom -- expected a BigDecimal but got " + x)
    }
    p += 1
    value
  }

  def readDouble(isMapKey: Boolean): Double = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number             => json.substring(jt.begin, jt.end).toDouble
      case String if isMapKey => json.substring(jt.begin, jt.end).toDouble
      case x                  => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readInt(isMapKey: Boolean): Int = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number             => json.substring(jt.begin, jt.end).toInt
      case String if isMapKey => json.substring(jt.begin, jt.end).toInt
      case x                  => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readLong(isMapKey: Boolean): Long = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number             => json.substring(jt.begin, jt.end).toLong
      case String if isMapKey => json.substring(jt.begin, jt.end).toLong
      case x                  => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readObjectFields[T](path: Path, fields: Map[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): (Boolean, Array[Any], Array[Boolean]) = {
    tokens(p).tokenType match {
      case BeginObject =>
        var fieldCount = 0
        p += 1
        val args = new Array[Any](fields.size)
        val flags = new Array[Boolean](fields.size)
        while (p < tokens.length && tokens(p).tokenType != EndObject) {
          val key = json.substring(tokens(p).asInstanceOf[JsonToken].begin, tokens(p).asInstanceOf[JsonToken].end)
          p += 1
          fields.get(key).map { oneField =>
            fields.get(key).map { f =>
              args(f.index) = oneField.valueTypeAdapter.read(path \ key, this, false)
              flags(f.index) = true
              fieldCount += 1
            }
          }
        }
        p += 1
        (fieldCount == fields.size, args, flags)
      case Null =>
        null
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        val subReader = this.cloneWithSource(js)
        p += 1
        subReader.readObjectFields(path, fields, isMapKey)
      case _ =>
        throw new Exception("Boom -- expected an Object but got " + tokens(p).tokenType)
    }
  }

  def readString(): String = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case String => json.substring(jt.begin, jt.end)
      case Null   => null
      case x      => throw new Exception("Boom -- expected a String but got " + x)
    }
    p += 1
    value
  }
}
