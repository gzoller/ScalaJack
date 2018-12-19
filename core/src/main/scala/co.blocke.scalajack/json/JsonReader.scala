package co.blocke.scalajack
package json

import model._
import model.TokenType._

import collection.mutable.ArrayBuffer
import scala.collection.immutable.{ ListMap, Map }
import scala.collection.generic.CanBuildFrom

case class JsonReader(json: String, tokens: ArrayBuffer[Token], tokenizer: Tokenizer[String] = JsonTokenizer()) extends Reader {

  type WIRE = String

  private var p: Int = 0

  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem], isMapKey: Boolean): To =
    tokens(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        while (p <= tokens.length && tokens(p).tokenType != EndArray) {
          p += 1
          builder += elementTypeAdapter.read(this, isMapKey)
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        val arrTokens = tokenizer.tokenize(js)
        p += 1
        JsonReader(js, arrTokens, tokenizer).readArray(canBuildFrom, elementTypeAdapter, false)
      case _ =>
        throw new Exception("Boom -- expected an Array but got " + tokens(p).tokenType)
    }

  def readMap[Key, Value, To](canBuildFrom: CanBuildFrom[_, (Key, Value), To], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], isMapKey: Boolean): To =
    tokens(p).tokenType match {
      case BeginObject =>
        val builder = canBuildFrom()
        while (p <= tokens.length && tokens(p).tokenType != EndObject) {
          p += 1
          val key = keyTypeAdapter.read(this, true)
          p += 1 // skip kv separator
          val value = valueTypeAdapter.read(this, false)
          builder += key -> value
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case String if isMapKey =>
        val jt = tokens(p).asInstanceOf[JsonToken]
        val js = json.substring(jt.begin, jt.end)
        val arrTokens = tokenizer.tokenize(js)
        p += 1
        JsonReader(js, arrTokens, tokenizer).readMap(canBuildFrom, keyTypeAdapter, valueTypeAdapter, false)
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

  def readObjectFields[T](fields: ListMap[String, ClassHelper.ClassFieldMember[T, Any]], isMapKey: Boolean): Map[String, Any] = {
    tokens(p).tokenType match {
      case BeginObject =>
        val builder = collection.mutable.Map.empty[String, Any]
        while (p <= tokens.length && tokens(p).tokenType != EndObject) {
          p += 1
          //          val jt = tokens(p).asInstanceOf[JsonToken]
          //          val key = json.substring(jt.begin, jt.end)
          //          p += 2 // skip kv separator
          //          builder.put(key, fields(key).valueTypeAdapter.read(this, false))
        }
        p += 1
        builder.toMap
      case Null =>
        null
      //      case String if isMapKey =>
      //        val jt = tokens(p).asInstanceOf[JsonToken]
      //        val js = json.substring(jt.begin, jt.end)
      //        val arrTokens = tokenizer.tokenize(js)
      //        p += 1
      //        JsonReader(js, arrTokens, tokenizer).readMap(canBuildFrom, keyTypeAdapter, valueTypeAdapter, false)
      case _ =>
        throw new Exception("Boom -- expected an Object but got " + tokens(p).tokenType)
    }
  }

  def readString(): String = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case String => json.substring(jt.begin, jt.end)
      case x      => throw new Exception("Boom -- expected a String but got " + x)
    }
    p += 1
    value
  }
}
