package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

class TokenReader(
    override val source: Array[Char],
    numberOfTokens:      Int,
    tokenTypes:          Array[TokenType],
    tokenOffsets:        Array[Int],
    tokenLengths:        Array[Int]
) extends Reader {

  override var position = -1

  override def peek: TokenType = tokenTypes(position + 1)

  override def read(expected: TokenType): Unit = {
    position += 1

    val actual = tokenTypes(position)
    if (actual != expected) {
      throw new IllegalStateException(s"Expected token of type $expected, not $actual")
    }
  }

  def unescapedTokenText: String = {
    var builder: StringBuilder = null
    var startOfUnescapedCharacters = 0

    val source = this.source
    val tokenOffset = this.tokenOffset
    val tokenLength = this.tokenLength

    var position = tokenOffset
    val maxPosition = tokenOffset + tokenLength

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
              val hexEncoded = new String(source, position + 2, position + 6)
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

  override def readString(): String = {
    read(expected = TokenType.String)
    unescapedTokenText
  }

  override def readIdentifier(): String = {
    read(expected = TokenType.Identifier)
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
