package co.blocke.scalajack
package csv

import TokenType.TokenType

class CSVTokenReader(
    override val source: Array[Char],
    numberOfTokens:      Int,
    tokenTypes:          Array[TokenType],
    tokenOffsets:        Array[Int],
    tokenLengths:        Array[Int]) extends Reader {

  override var position = -1

  override def peek: TokenType = tokenTypes(position + 1)

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
    val source = this.source
    val tokenOffset = this.tokenOffset
    val tokenLength = this.tokenLength

    val builder = new StringBuilder(tokenLength)

    val minPosition = tokenOffset
    val maxPosition = tokenOffset + tokenLength

    var position = minPosition

    while (position < maxPosition) {
      source(position) match {
        case '"' =>
          builder.append('"')
          position += 2

        case ch =>
          builder.append(ch)
          position += 1
      }
    }
    builder.toString()
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

  // $COVERAGE-OFF$Never used for CSV
  override def captureValue(): Any = null
  // $COVERAGE-ON$

  override def tokenOffsetAt(position: Int): Int = tokenOffsets(position)

  override def tokenLengthAt(position: Int): Int = tokenLengths(position)

}
