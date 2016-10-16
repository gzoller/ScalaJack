package co.blocke.scalajack

import co.blocke.scalajack.TokenType.TokenType
import org.bson.BsonValue

class BsonReader(
    numberOfTokens: Int,
    tokenTypes:     Array[TokenType],
    strings:        Array[String],
    values:         Array[BsonValue]
) extends Reader {

  override var position: Int = -1

  override def source: Array[Char] = ???

  override def tokenOffsetAt(position: Int): Int = ???

  override def tokenLengthAt(position: Int): Int = ???

  override def peek: TokenType = tokenTypes(position + 1)

  override def read(): TokenType = {
    position += 1
    tokenTypes(position)
  }

  override def read(expected: TokenType): Unit = {
    position += 1
    if (expected != tokenTypes(position)) {
      throw new Exception("Wrong token type")
    }
  }

  override def readString(): String = {
    read(expected = TokenType.String)
    strings(position)
  }

  override def readInt(): Int = {
    read(expected = TokenType.Number)
    values(position).asInt32.intValue
  }

  override def readLong(): Long = {
    read(expected = TokenType.Number)
    values(position).asInt64.longValue
  }

  override def tokenText: String = "TOKEN TEXT"

  override def showError(): String = "SOMETHING WENT WRONG"

  override def captureValue(): Any = {
//    val startTok = position + 1
//    skipValue()
//    val endTok = Math.max(startTok, position)
//    new String(source.slice(tokenOffsets(startTok), tokenOffsets(endTok) + tokenLengths(endTok)))
    ???
  }

}
