package co.blocke.scalajack
package json

import TokenType.TokenType

class Tokenizer(val isCanonical: Boolean = true, val capacity: Int = 1024) {

  def showError(): String = ""

  // private var position: Int = 0
  // private var source = Array.empty[Char]

  def tokenize(src: Array[Char], offset: Int, length: Int, capacity: Int = 1024): TokenReader = {

    val maxPosition = offset + length
    var position = offset
    val source = src

    val tokenTypes = new Array[TokenType](capacity)
    val tokenOffsets = new Array[Int](capacity)
    val tokenLengths = new Array[Int](capacity)

    var numberOfTokens = 0

    def showError(): String = {
      val startPosOffset = if (position - 50 < 0) position else 50
      val startPos = position - startPosOffset
      val endPos = if (position + 50 > source.length) source.length else position + 50
      val buf = new StringBuffer()
      buf.append(source.subSequence(startPos, endPos).toString + "\n")
      buf.append("-" * startPosOffset + "^")
      buf.toString
    }

    @inline def appendToken(tokenType: TokenType, tokenOffset: Int, tokenLength: Int): Unit = {
      tokenTypes(numberOfTokens) = tokenType
      tokenOffsets(numberOfTokens) = tokenOffset
      tokenLengths(numberOfTokens) = tokenLength
      numberOfTokens += 1
    }

    @inline def isInitialLiteralNameChar(ch: Char): Boolean = ch.isLetter || ch == '_'
    @inline def isSubsequentLiteralNameChar(ch: Char): Boolean = ch.isLetter || ch == '_' || ch.isDigit
    @inline def isIntegerChar(ch: Char): Boolean = ('0' <= ch && ch <= '9') || ch == '.' || ch == '-' || ch == '+' || ch == 'e' || ch == 'E'

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

        case '"' ⇒
          val start = position

          position += 1 // Skip the leading double-quote

          while (position < maxPosition && source(position) != '"') {
            if (source(position) == '\\') {
              position += 2
            } else {
              position += 1
            }
          }
          // if (position == maxPosition) throw new IllegalArgumentException("Unterminated string")
          if (position == maxPosition) throw new IllegalArgumentException("Unterminated string\n" + showError())

          position += 1 // Pass over the closing double-quote
          appendToken(TokenType.String, start, position - start)

        case ' ' ⇒ // skip whitespace
          position += 1

        case '\r' ⇒ // skip whitespace
          position += 1

        case '\n' ⇒ // skip whitespace
          position += 1

        case '\t' ⇒ // skip whitespace
          position += 1

        case ch ⇒ // Tokenize some literal
          // Integer
          if (isIntegerChar(ch)) {
            val start = position

            while (position < maxPosition && isIntegerChar(source(position))) {
              position += 1
            }

            appendToken(TokenType.Number, start, position - start)
          } else if (isInitialLiteralNameChar(ch)) { // Literal name
            val literalNameOffset = position

            position += 1 // Skip initial character

            while (position < maxPosition && isSubsequentLiteralNameChar(source(position))) {
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

          } else {
            // throw new IllegalArgumentException(s"Unknown character: $ch")
            throw new IllegalArgumentException(s"Unknown character: $ch\n" + showError())
          }
      }
    }
    appendToken(TokenType.End, position, 0)

    new TokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }

}
