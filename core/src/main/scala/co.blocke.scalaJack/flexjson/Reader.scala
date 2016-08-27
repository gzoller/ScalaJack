package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

trait Reader {

  var position: Int

  def source: Array[Char]

  def tokenOffset: Int

  def tokenLength: Int

  def peek: TokenType

  def read(): TokenType

  def read(expected: TokenType): Unit

  def readString(): String

  def skipValue(): Unit = {
    peek match {
      case TokenType.BeginObject =>
        // TODO
        ???

      case TokenType.BeginArray =>
        // TODO
        ???

      case _ =>
        position += 1
    }
  }

  def tokenText: String

  def hasMoreElements: Boolean = peek != TokenType.EndArray

  def hasMoreFields: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
