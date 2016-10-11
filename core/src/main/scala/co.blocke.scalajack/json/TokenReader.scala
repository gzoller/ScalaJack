package co.blocke.scalajack
package json

import TokenType.TokenType

class TokenReader(
    override val source: Array[Char],
    numberOfTokens:      Int,
    tokenTypes:          Array[TokenType],
    tokenOffsets:        Array[Int],
    tokenLengths:        Array[Int]
) extends Reader {

  override var position = -1

  override def peek: TokenType = tokenTypes(position + 1)
  def poke(tt: TokenType, newTokenOffset: Int => Int, newTokenLength: Int => Int) = {
    val nextPosition = position + 1
    tokenTypes(nextPosition) = tt
    tokenOffsets(nextPosition) = newTokenOffset(tokenOffsets(nextPosition))
    tokenLengths(nextPosition) = newTokenLength(tokenLengths(nextPosition))
  }

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

    val minPosition = tokenOffset + 1 // ignore the leading double-quote
    val maxPosition = tokenOffset + tokenLength - 1 // ignore the trailing double-quote

    var position = minPosition

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
      new String(source, minPosition, maxPosition - minPosition)
    } else {
      builder.appendAll(source, startOfUnescapedCharacters, maxPosition - startOfUnescapedCharacters)
      builder.toString()
    }
  }

  override def captureValue(): Any = {
    val startTok = position + 1
    skipValue()
    val endTok = Math.max(startTok, position)
    new String(source.slice(tokenOffsets(startTok), tokenOffsets(endTok) + tokenLengths(endTok)))
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
