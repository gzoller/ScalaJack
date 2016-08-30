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

  def readNull[T]()(implicit ev: Null <:< T): T = {
    read(expected = TokenType.Null)
    ev(null)
  }

  def readIdentifier(): String

  def skipValue(): Unit = {
    peek match {
      case TokenType.BeginObject ⇒ skipOver(TokenType.BeginObject, TokenType.EndObject)

      case TokenType.BeginArray  ⇒ skipOver(TokenType.BeginArray, TokenType.EndArray)

      case _ ⇒
        position += 1
    }
  }

  private def skipOver(beginToken: TokenType.Value, endToken: TokenType.Value): Unit = {
    var depth = 0
    while (depth > 0 || peek != endToken) {
      println(s"Peek ${depth}:${position}: ${peek}")
      position += 1
      peek match {
        case `beginToken` ⇒
          depth += 1
        case `endToken` if (depth > 0) ⇒
          depth -= 1
          position += 1
        case _ ⇒
      }
    }
    position += 1
  }

  def tokenText: String

  def hasNext: Boolean = peek != TokenType.End // differentiate end-of-parsing from end of object/array

  def hasMoreElements: Boolean = peek != TokenType.EndArray

  def hasMoreFields: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
