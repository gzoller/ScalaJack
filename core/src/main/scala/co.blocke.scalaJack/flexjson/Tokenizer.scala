package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

class Tokenizer(val capacity: Int = 1024) {

  @inline final val ObjectStructureType: Byte = 1
  @inline final val ArrayStructureType: Byte = 2

  def tokenize(source: Array[Char], offset: Int, length: Int, capacity: Int = 1024): TokenReader = {
    val maxPosition = offset + length
    var position = offset

    val tokenTypes = new Array[TokenType](capacity)
    val tokenOffsets = new Array[Int](capacity)
    val tokenLengths = new Array[Int](capacity)

    var numberOfTokens = 0
    var isIdentifier = false

    val structureTypeStack = new Array[Byte](256)
    var indexOfTopStructure = -1

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
          indexOfTopStructure += 1 // stack push
          structureTypeStack(indexOfTopStructure) = ObjectStructureType
          appendToken(TokenType.BeginObject, position, 1)
          position += 1
          isIdentifier = true

        case '}' ⇒
          indexOfTopStructure -= 1 // stack pop
          appendToken(TokenType.EndObject, position, 1)
          position += 1

        case '[' ⇒
          indexOfTopStructure += 1 // stack push
          structureTypeStack(indexOfTopStructure) = ArrayStructureType
          appendToken(TokenType.BeginArray, position, 1)
          position += 1

        case ']' ⇒
          indexOfTopStructure -= 1 // stack pop
          appendToken(TokenType.EndArray, position, 1)
          position += 1

        case ':' ⇒
          position += 1

        case ',' ⇒
          position += 1
          if (structureTypeStack(indexOfTopStructure) == ObjectStructureType) // , inside object is a field separator... identifier expected
            isIdentifier = true

        case ' ' ⇒ // skip whitespace
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
            } else
              position += 1
          }

          if (isIdentifier) {
            appendToken(TokenType.Identifier, start, position - start)
            isIdentifier = false
          } else {
            appendToken(TokenType.String, start, position - start)
          }

          position += 1 // Skip the trailing double-quote

        case 'n' ⇒ // HUGE assumption this is null, but checking would slow us down too much
          appendToken(TokenType.Null, position, 4)
          position += 4

        case 't' ⇒ // HUGE assumption this is true, but checking would slow us down too much
          appendToken(TokenType.True, position, 4)
          position += 4

        case 'f' ⇒ // HUGE assumption this is false, but checking would slow us down too much
          appendToken(TokenType.False, position, 5)
          position += 5

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

            appendToken(TokenType.LiteralName, literalNameOffset, literalNameLength)
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
