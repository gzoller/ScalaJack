package co.blocke.scalajack
package json

import TokenType.TokenType

class Tokenizer(val capacity: Int = 1024) {

  def tokenize(source: Array[Char], offset: Int, length: Int, capacity: Int = 1024): TokenReader = {
    val maxPosition = offset + length
    var position = offset

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

    @inline def isLetter(ch: Char): Boolean = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

    @inline def isDigit(ch: Char): Boolean = '0' <= ch && ch <= '9'

    @inline def isSign(ch: Char): Boolean = ch == '+' || ch == '-'

    @inline def isDecimalPoint(ch: Char): Boolean = ch == '.'

    @inline def isUnderscore(ch: Char): Boolean = ch == '_'

    @inline def isInitialLiteralNameChar(ch: Char): Boolean = isLetter(ch)

    @inline def isSubsequentLiteralNameChar(ch: Char): Boolean = isLetter(ch) || isUnderscore(ch) || isDigit(ch)

    @inline def isIntegerChar(ch: Char): Boolean = ('0' <= ch && ch <= '9') || ch == '.' || ch == '-' || ch == '+' || ch == 'e' || ch == 'E'

    @inline def isE(ch: Char): Boolean = ch == 'e' || ch == 'E'

    @inline def skipInteger(): Boolean =
      if (isSign(source(position))) {
        position += 1

        while (isDigit(source(position))) {
          position += 1
        }

        true
      } else if (isDigit(source(position))) {
        while (isDigit(source(position))) {
          position += 1
        }

        true
      } else {
        false
      }

    @inline def skipFraction(): Boolean =
      if (isDecimalPoint(source(position))) {
        position += 1

        while (isDigit(source(position))) {
          position += 1
        }

        true
      } else {
        false
      }

    @inline def skipExponent(): Boolean = {
      if (isE(source(position))) {
        position += 1

        skipInteger()

        true
      } else {
        false
      }
    }

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

        case ' ' ⇒ // skip whitespace
          position += 1

        case '\r' ⇒ // skip whitespace
          position += 1

        case '\n' ⇒ // skip whitespace
          position += 1

        case '\t' ⇒ // skip whitespace
          position += 1

        case '"' ⇒
          position += 1 // Skip the leading double-quote

          val start = position

          while (source(position) != '"') {
            if (source(position) == '\\') {
              position += 2
            } else {
              position += 1
            }
          }

          appendToken(TokenType.String, start, position - start)
          position += 1 // Skip the trailing double-quote

        case ch ⇒
          // Integer
          if (isIntegerChar(ch)) {
            val start = position

            while (isIntegerChar(source(position))) {
              position += 1
            }

            appendToken(TokenType.Number, start, position - start)
          } else if (isInitialLiteralNameChar(ch)) { // Literal name
            val literalNameOffset = position

            position += 1 // Skip initial character

            while (isSubsequentLiteralNameChar(source(position))) {
              position += 1
            }

            val literalNameLength = position - literalNameOffset

            if (literalNameLength == 4
              && source(literalNameOffset + 0) == 'n'
              && source(literalNameOffset + 1) == 'u'
              && source(literalNameOffset + 2) == 'l'
              && source(literalNameOffset + 3) == 'l') {
              appendToken(TokenType.Null, literalNameOffset, literalNameLength)
            } else if (literalNameLength == 4
              && source(literalNameOffset + 0) == 't'
              && source(literalNameOffset + 1) == 'r'
              && source(literalNameOffset + 2) == 'u'
              && source(literalNameOffset + 3) == 'e') {
              appendToken(TokenType.True, literalNameOffset, literalNameLength)
            } else if (literalNameLength == 5
              && source(literalNameOffset + 0) == 'f'
              && source(literalNameOffset + 1) == 'a'
              && source(literalNameOffset + 2) == 'l'
              && source(literalNameOffset + 3) == 's'
              && source(literalNameOffset + 4) == 'e') {
              appendToken(TokenType.False, literalNameOffset, literalNameLength)
            } else {
              appendToken(TokenType.UnknownLiteralName, literalNameOffset, literalNameLength)
            }
          } else if (isSign(ch)) {

          } else if (isDigit(ch)) {

          } else if (isDecimalPoint(ch)) {

          } else {
            throw new IllegalArgumentException(s"Unknown character: $ch")
          }
      }
    }
    appendToken(TokenType.End, position, 0)

    new TokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }

}
