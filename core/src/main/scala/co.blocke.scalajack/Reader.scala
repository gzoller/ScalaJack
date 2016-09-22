package co.blocke.scalajack

import TokenType.TokenType

trait Reader {

  var position: Int

  def source: Array[Char]

  def tokenOffset: Int = tokenOffsetAt(position)

  def tokenOffsetAt(position: Int): Int

  def tokenLength: Int = tokenLengthAt(position)

  def tokenLengthAt(position: Int): Int

  def peek: TokenType

  def read(): TokenType

  def read(expected: TokenType): Unit

  def readString(): String

  def readNull[T]()(implicit ev: Null <:< T): T = {
    read(expected = TokenType.Null)
    ev(null)
  }

  def readBoolean(): Boolean = {
    peek match {
      case TokenType.False ⇒
        false

      case TokenType.True ⇒
        true
    }
  }

  def readNumber(): java.lang.Number = {
    read(expected = TokenType.Number)
    val tokenText = this.tokenText
    java.lang.Integer.valueOf(tokenText)
  }

  def readByte(): Byte = {
    read(expected = TokenType.Number)
    tokenText.toByte
  }

  def readShort(): Short = {
    read(expected = TokenType.Number)
    tokenText.toShort
  }

  def readInt(): Int = {
    read(expected = TokenType.Number)
    tokenText.toInt
  }

  def readLong(): Long = {
    read(expected = TokenType.Number)
    tokenText.toLong
  }

  def readFloat(): Float = {
    read(expected = TokenType.Number)
    tokenText.toFloat
  }

  def readDouble(): Double = {
    read(expected = TokenType.Number)
    tokenText.toDouble
  }

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

  def hasMoreMembers: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
