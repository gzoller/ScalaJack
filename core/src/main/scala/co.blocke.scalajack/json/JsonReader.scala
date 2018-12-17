package co.blocke.scalajack
package json

import model._
import model.TokenType._

import collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom

case class JsonReader(json: String, tokens: ArrayBuffer[Token]) extends Reader {

  private var p: Int = 0

  def readArray[Elem, To](canBuildFrom: CanBuildFrom[_, Elem, To], elementTypeAdapter: TypeAdapter[Elem]): To =
    tokens(p).tokenType match {
      case BeginArray =>
        val builder = canBuildFrom()
        while (p <= tokens.length && tokens(p).tokenType != EndArray) {
          p += 1
          builder += elementTypeAdapter.read(this)
        }
        p += 1
        builder.result
      case Null =>
        null.asInstanceOf[To]
      case _ =>
        throw new Exception("Boom -- expected an Array but got " + tokens(p).tokenType)
    }

  def readBoolean(): Boolean = {
    val value = tokens(p).tokenType match {
      case True  => true
      case False => false
      case x     => throw new Exception("Boom -- expected a Boolean but got " + x)
    }
    p += 1
    value
  }

  def readDecimal(): BigDecimal = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number => BigDecimal(json.substring(jt.begin, jt.end))
      case Null   => null
      case x      => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readDouble(): Double = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number => json.substring(jt.begin, jt.end).toDouble
      case x      => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readInt(): Int = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number => json.substring(jt.begin, jt.end).toInt
      case x      => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
  }

  def readLong(): Long = {
    val jt = tokens(p).asInstanceOf[JsonToken]
    val value = jt.tokenType match {
      case Number => json.substring(jt.begin, jt.end).toLong
      case x      => throw new Exception("Boom -- expected a Number but got " + x)
    }
    p += 1
    value
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
