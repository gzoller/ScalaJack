package co.blocke.scalajack
package json

import TokenType.TokenType

class TokenReader(
    override val source: Array[Char],
    numberOfTokens:      Int,
    val tokenTypes:      Array[TokenType],
    val tokenOffsets:    Array[Int],
    tokenLengths:        Array[Int]
) extends Reader {

  override var position = -1

  override def peek: TokenType = tokenTypes(position + 1)
  def poke(tt: TokenType) = tokenTypes(position + 1) = tt

  private[scalajack] def getTokens() = tokenTypes.take(numberOfTokens).toList

  override def showError(): String = {
    val charPos = tokenOffsets(position)
    val startPosOffset = if (charPos - 50 < 0) charPos else 50
    val startPos = charPos - startPosOffset
    val endPos = if (charPos + 50 > source.length) source.length else charPos + 50
    val buf = new StringBuffer()
    buf.append(source.subSequence(startPos, endPos).toString + "\n")
    buf.append("-" * startPosOffset + "^")
    buf.toString
  }

  override def read(expected: TokenType): Unit = {
    position += 1

    val actual = tokenTypes(position)
    if (actual != expected) {
      throw new IllegalStateException(s"Expected token of type $expected, not $actual\n" + showError())
    }
  }

  def unescapedTokenText: String = {
    var builder: StringBuilder = null

    val source = this.source
    val tokenOffset = this.tokenOffset
    val tokenLength = this.tokenLength

    var position = tokenOffset
    val maxPosition = tokenOffset + tokenLength

    var startOfUnescapedCharacters = position

    while (position < maxPosition) {
      source(position) match {
        case '\\' ⇒

          if (builder == null) builder = new StringBuilder(tokenLength)

          builder.appendAll(source, startOfUnescapedCharacters, position - startOfUnescapedCharacters)

          source(position + 1) match {
            case '"' ⇒
              builder.append('"')
              position += 2

            case '\\' ⇒
              builder.append('\\')
              position += 2

            case '/' ⇒
              builder.append('/')
              position += 2

            case 'b' ⇒
              builder.append('\b')
              position += 2

            case 'f' ⇒
              builder.append('\f')
              position += 2

            case 'n' ⇒
              builder.append('\n')
              position += 2

            case 'r' ⇒
              builder.append('\r')
              position += 2

            case 't' ⇒
              builder.append('\t')
              position += 2

            case 'u' ⇒
              val hexEncoded = new String(source, position + 2, 4)
              val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
              builder.append(unicodeChar)
              position += 6
          }

          startOfUnescapedCharacters = position

        case ch ⇒
          position += 1
      }
    }

    if (builder == null) {
      tokenText
    } else {
      builder.appendAll(source, startOfUnescapedCharacters, maxPosition - startOfUnescapedCharacters)
      builder.toString()
    }
  }

  override def captureValue(): Any = {
    val startPos = tokenOffsets(position + 1)
    source(startPos) match {
      case '{' ⇒
        skipContext('}', startPos)
      case '[' ⇒
        skipContext(']', startPos)
      case c if (source(startPos - 1) == '"') ⇒
        read(expected = TokenType.String)
        '"' + tokenText + '"'
      case c ⇒ // literal
        skipContext(',', startPos).reverse.tail.reverse.trim
    }
  }
  private def skipContext(c: Char, startPos: Int) = {
    skipValue()
    var endPos = tokenOffsets(position + 1)
    while (source(endPos) != c && source(endPos) != '}' && endPos > 0) endPos -= 1
    new String(source, startPos, (endPos + 1) - startPos)
  }

  override def readString(): String = {
    read(expected = TokenType.String)
    unescapedTokenText
  }

  override def tokenText: String =
    new String(source, tokenOffsets(position), tokenLengths(position))

  override def read(): TokenType = {
    position += 1
    tokenTypes(position)
  }

  override def tokenOffsetAt(position: Int): Int = tokenOffsets(position)

  override def tokenLengthAt(position: Int): Int = tokenLengths(position)

}
