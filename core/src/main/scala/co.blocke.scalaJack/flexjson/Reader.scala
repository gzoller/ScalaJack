package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

trait Reader {

  def peek: TokenType

  def read(expected: TokenType): Unit

  def readString(): String

  def skipValue(): Unit = ???

  def tokenText: String

  def hasMoreElements: Boolean = peek != TokenType.EndArray

  def hasMoreFields: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
