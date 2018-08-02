package co.blocke.scalajack
package csv

import TokenType.TokenType

class Tokenizer() { //(val capacity: Int = 1024) {

  def tokenize(source: Array[Char], offset: Int, length: Int, initialCapacity: Int = 256): CSVTokenReader = {
    val maxPosition = offset + length
    var position = offset

    var capacity = if (initialCapacity > 0) initialCapacity else 1
    println("Cap: " + capacity)
    var tokenTypes = new Array[TokenType](capacity)
    var tokenOffsets = new Array[Int](capacity)
    var tokenLengths = new Array[Int](capacity)

    var numberOfTokens = 0

    @inline def appendToken(tokenType: TokenType, tokenOffset: Int, tokenLength: Int): Unit = {
      println("Append Token " + tokenType + " @ " + tokenOffset)
      if (numberOfTokens == capacity) {
        // val oldCapacity = capacity
        val newCapacity = capacity * 2
        println("Bump!  " + newCapacity)

        val oldTokenTypes = tokenTypes
        val newTokenTypes = new Array[TokenType](newCapacity)
        Array.copy(oldTokenTypes, 0, newTokenTypes, 0, capacity)
        tokenTypes = newTokenTypes

        val oldTokenOffsets = tokenOffsets
        val newTokenOffsets = new Array[Int](newCapacity)
        Array.copy(oldTokenOffsets, 0, newTokenOffsets, 0, capacity)
        tokenOffsets = newTokenOffsets

        val oldTokenLengths = tokenLengths
        val newTokenLengths = new Array[Int](newCapacity)
        Array.copy(oldTokenLengths, 0, newTokenLengths, 0, capacity)
        tokenLengths = newTokenLengths

        capacity = newCapacity
      }

      tokenTypes(numberOfTokens) = tokenType
      tokenOffsets(numberOfTokens) = tokenOffset
      tokenLengths(numberOfTokens) = tokenLength
      numberOfTokens += 1
    }

    if (length == 0)
      appendToken(TokenType.Null, 0, 0)
    else {
      appendToken(TokenType.BeginObject, 0, 0)
      while (position < maxPosition) {
        source(position) match {
          case ',' =>
            if (position == offset || source(position - 1) == ',') // account for empty field
              appendToken(TokenType.Null, position, 0)
            position += 1 // skip comma
          case '"' =>
            val savePos = position + 1
            do {
              position += 1
              if (position < maxPosition - 1 && source(position) == '"' && source(position + 1) == '"') position += 2 // skip escaped quote
            } while (position < maxPosition && source(position) != '"')
            appendToken(TokenType.String, savePos, position - savePos)
            if (position < maxPosition) position += 1
          case c =>
            val savePos = position
            do {
              position += 1
            } while (position < maxPosition && source(position) != ',')
            inferKind(source, savePos, position - 1)
        }
      }
      if (source(position - 1) == ',')
        appendToken(TokenType.Null, position - 1, 0)
      appendToken(TokenType.EndObject, position, 0)
    }
    appendToken(TokenType.End, position, 0)

    @inline def isNumberChar(ch: Char): Boolean = ('0' <= ch && ch <= '9') || ch == '.' || ch == '-' || ch == '+' || ch == 'e' || ch == 'E'

    def inferKind(arr: Array[Char], start: Int, end: Int) {
      if (end + 1 - start == 4
        && source(start + 0) == 't'
        && source(start + 1) == 'r'
        && source(start + 2) == 'u'
        && source(start + 3) == 'e') {
        appendToken(TokenType.True, start, 4)
      } else if (end + 1 - start == 5
        && source(start + 0) == 'f'
        && source(start + 1) == 'a'
        && source(start + 2) == 'l'
        && source(start + 3) == 's'
        && source(start + 4) == 'e') {
        appendToken(TokenType.False, start, 5)
      } else if (arr.slice(start, (end + 1)).foldLeft(true) { case (acc, ch) => acc && isNumberChar(ch) }) {
        appendToken(TokenType.Number, start, end + 1 - start)
      } else {
        appendToken(TokenType.String, start, end + 1 - start)
      }
    }

    new CSVTokenReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }
}
