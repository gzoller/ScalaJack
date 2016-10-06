package co.blocke.scalajack
package json

import TokenType.TokenType

class Tokenizer(val capacity: Int = 1024) {

  // RawContext == 0
  private val ArrayContext: Int = 1
  private val ObjectContext: Int = ArrayContext << 1
  private val ExpectKey: Int = ObjectContext << 1
  private val ExpectValue: Int = ExpectKey << 1
  private val ExpectColon: Int = ExpectValue << 1
  private val ExpectComma: Int = ExpectColon << 1
  private val ExpectEndOfStructure: Int = ExpectComma << 1

  def showError(): String = {
    val startPosOffset = if (position - 50 < 0) position else 50
    val startPos = position - startPosOffset
    val endPos = if (position + 50 > source.length) source.length else position + 50
    val buf = new StringBuffer()
    buf.append(source.subSequence(startPos, endPos).toString + "\n")
    buf.append("-" * startPosOffset + "^")
    buf.toString
  }

  private var position: Int = 0
  private var source = Array.empty[Char]

  def tokenize(src: Array[Char], offset: Int, length: Int, capacity: Int = 1024): TokenReader = {
    val maxPosition = offset + length
    position = offset
    source = src

    val tokenTypes = new Array[TokenType](capacity)
    val tokenOffsets = new Array[Int](capacity)
    val tokenLengths = new Array[Int](capacity)

    var numberOfTokens = 0

    val validate = new Array[Int](500)
    var validPos = 0
    setValidBit(ExpectValue)

    @inline def setValidBit(bit: Int) = validate(validPos) |= bit
    @inline def unsetValidBit(bit: Int) = validate(validPos) &= ~bit
    @inline def isValidClear = validate(validPos) == 0
    @inline def isValidSet(bit: Int) = (validate(validPos) | bit) == validate(validPos)
    @inline def pushValid() {
      validPos += 1
      validate(validPos) = 0
    }
    @inline def popValid() {
      if (validate(validPos) != 0) throw new IllegalArgumentException("Unfinished business: " + validate(validPos))
      validPos -= 1
    }

    @inline def appendToken(tokenType: TokenType, tokenOffset: Int, tokenLength: Int): Unit = {
      val i = numberOfTokens
      numberOfTokens += 1

      tokenTypes(i) = tokenType
      tokenOffsets(i) = tokenOffset
      tokenLengths(i) = tokenLength
    }

    @inline def isLetter(ch: Char): Boolean = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

    @inline def isDigit(ch: Char): Boolean = '0' <= ch && ch <= '9'

    // Unused at present
    /*
    @inline def isSign(ch: Char): Boolean = ch == '+' || ch == '-'

    @inline def isDecimalPoint(ch: Char): Boolean = ch == '.'
    */

    @inline def isUnderscore(ch: Char): Boolean = ch == '_'

    @inline def isInitialLiteralNameChar(ch: Char): Boolean = isLetter(ch) || isUnderscore(ch)

    @inline def isSubsequentLiteralNameChar(ch: Char): Boolean = isLetter(ch) || isUnderscore(ch) || isDigit(ch)

    @inline def isIntegerChar(ch: Char): Boolean = ('0' <= ch && ch <= '9') || ch == '.' || ch == '-' || ch == '+' || ch == 'e' || ch == 'E'

    @inline def isE(ch: Char): Boolean = ch == 'e' || ch == 'E'

    /* Unused at present...
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
    */

    while (position < maxPosition) {
      source(position) match {
        case '{' ⇒
          if (!isValidSet(ExpectValue)) throw new IllegalArgumentException("Character out of place. '{' not expected here.\n" + showError())
          pushValid()
          setValidBit(ObjectContext)
          setValidBit(ExpectKey)
          setValidBit(ExpectEndOfStructure)
          appendToken(TokenType.BeginObject, position, 1)
          position += 1

        case '}' ⇒
          if ((!isValidSet(ExpectEndOfStructure) && !isValidSet(ExpectComma) && !isValidSet(ExpectKey)) || isValidSet(ArrayContext)) throw new IllegalArgumentException("Character out of place. '}' not expected here.\n" + showError())
          unsetValidBit(ObjectContext)
          unsetValidBit(ExpectComma)
          unsetValidBit(ExpectKey)
          unsetValidBit(ExpectEndOfStructure)
          popValid()
          unsetValidBit(ExpectValue)
          if (isValidSet(ArrayContext) || isValidSet(ObjectContext))
            setValidBit(ExpectComma)

          appendToken(TokenType.EndObject, position, 1)
          position += 1

        case '[' ⇒
          if (!isValidSet(ExpectValue)) throw new IllegalArgumentException("Character out of place. '[' not expected here.\n" + showError())
          pushValid()
          setValidBit(ArrayContext)
          setValidBit(ExpectValue)
          setValidBit(ExpectEndOfStructure)
          appendToken(TokenType.BeginArray, position, 1)
          position += 1

        case ']' ⇒
          if ((!isValidSet(ExpectEndOfStructure) && !isValidSet(ExpectComma)) || isValidSet(ObjectContext)) throw new IllegalArgumentException("Character out of place. ']' not expected here.\n" + showError())
          unsetValidBit(ArrayContext)
          unsetValidBit(ExpectComma)
          unsetValidBit(ExpectValue)
          unsetValidBit(ExpectEndOfStructure)
          popValid()
          unsetValidBit(ExpectValue)
          if (isValidSet(ArrayContext) || isValidSet(ObjectContext))
            setValidBit(ExpectComma)

          appendToken(TokenType.EndArray, position, 1)
          position += 1

        case ':' ⇒
          if (!isValidSet(ExpectColon)) throw new IllegalArgumentException("Character out of place. ':' not expected here.\n" + showError())
          unsetValidBit(ExpectColon)
          setValidBit(ExpectValue)
          position += 1

        case ',' ⇒
          if (!isValidSet(ExpectComma)) throw new IllegalArgumentException("Character out of place. ',' not expected here.\n" + showError())
          unsetValidBit(ExpectComma)
          if (isValidSet(ObjectContext))
            setValidBit(ExpectKey)
          else
            setValidBit(ExpectValue)
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
          if (!isValidSet(ExpectKey) && !isValidSet(ExpectValue)) throw new IllegalArgumentException("Character out of place. String not expected here.\n" + showError())

          val start = position

          position += 1 // Skip the leading double-quote

          while (position < maxPosition && source(position) != '"') {
            if (source(position) == '\\') {
              position += 2
            } else {
              position += 1
            }
          }
          if (position == maxPosition) throw new IllegalArgumentException("Unterminated string\n" + showError())

          position += 1 // Skip the trailing double-quote
          appendToken(TokenType.String, start, position - start)

          if (isValidSet(ExpectKey)) {
            unsetValidBit(ExpectKey)
            unsetValidBit(ExpectEndOfStructure)
            setValidBit(ExpectColon)
          } else { // value 
            unsetValidBit(ExpectValue)
            if (!isValidClear)
              setValidBit(ExpectComma)
          }

        case ch ⇒ // Tokenize some literal
          if (!isValidSet(ExpectValue)) throw new IllegalArgumentException("Character out of place. Un-quoted literal not expected here.  (Possile un-terminated string earlier in your JSON.)\n" + showError())
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
            throw new IllegalArgumentException(s"Unknown character: $ch\n" + showError())
          }
          unsetValidBit(ExpectValue)
          unsetValidBit(ExpectEndOfStructure)
          if (!isValidClear)
            setValidBit(ExpectComma)
      }
    }
    if (validPos > 0) {
      if (isValidSet(ArrayContext))
        throw new IllegalArgumentException("Unterminated array\n" + showError())
      else
        throw new IllegalArgumentException("Unterminated object\n" + showError())
    }
    appendToken(TokenType.End, position, 0)

    new TokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }

}
