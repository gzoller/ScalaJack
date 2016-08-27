package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

class Tokenizer {

  def tokenize(source: Array[Char], offset: Int, length: Int): TokenReader = {
    val maxPosition = offset + length
    var position = offset

    val capacity = 1024
    val tokenTypes = new Array[TokenType](capacity)
    val tokenOffsets = new Array[Int](capacity)
    val tokenLengths = new Array[Int](capacity)

    var numberOfTokens = 0

    @inline def appendToken(tokenType: TokenType, tokenOffset: Int, tokenLength: Int): Unit = {
      val i = numberOfTokens
      numberOfTokens += 1

      tokenTypes(i) = tokenType
      tokenOffsets(i) = tokenOffset
      tokenLengths(i) = tokenLength
    }

    @inline def isInitialIdentifierChar(ch: Char): Boolean = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

    @inline def isSubsequentIdentifierChar(ch: Char): Boolean = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ('0' <= ch && ch <= '9') || ch == '_'

    @inline def isIntegerChar(ch: Char): Boolean = '0' <= ch && ch <= '9'

    while (position < maxPosition) {
      source(position) match {
        case '{' ⇒
          appendToken(TokenType.BeginObject, position, 1)
          position += 1

        case '}' ⇒
          appendToken(TokenType.EndObject, position, 1)
          position += 1

        case '[' ⇒
          appendToken(TokenType.BeginArray, position, 1)
          position += 1

        case ']' ⇒
          appendToken(TokenType.EndArray, position, 1)
          position += 1

        case ':' ⇒
          position += 1

        case ',' ⇒
          position += 1

        case '\n' ⇒
          position += 1

        case '"' ⇒
          position += 1 // Skip the leading double-quote

          val start = position

          while (source(position) != '"') {
            position += 1
          }

          appendToken(TokenType.String, start, position - start)

          position += 1 // Skip the trailing double-quote

        case ch ⇒
          if (isInitialIdentifierChar(ch)) {
            val start = position
            position += 1

            while (isSubsequentIdentifierChar(source(position))) {
              position += 1
            }

            appendToken(TokenType.Identifier, start, position - start)
          } else if (isIntegerChar(ch)) {
            val start = position

            while (isIntegerChar(source(position))) {
              position += 1
            }

            appendToken(TokenType.Number, start, position - start)
          } else {
            throw new IllegalArgumentException(s"Unknown character: $ch")
          }

      }
    }

    new TokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }

}
