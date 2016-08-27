package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

class TokenReader(source: Array[Char],
                  numberOfTokens: Int,
                  tokenTypes: Array[TokenType],
                  tokenOffsets: Array[Int],
                  tokenLengths: Array[Int]) extends Reader {

  var position = -1

  override def peek: TokenType = tokenTypes(position + 1)

  override def read(expected: TokenType): Unit = {
    position += 1

    val actual = tokenTypes(position)
    if (actual != expected) {
      throw new IllegalStateException(s"Expected token of type $expected, not $actual")
    }
  }

  override def readString(): String = {
    read(expected = TokenType.String)
    tokenText
  }

  override def tokenText: String =
    new String(source, tokenOffsets(position), tokenLengths(position))

}
