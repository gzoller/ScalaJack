package co.blocke.scalajack
package mongo

import co.blocke.scalajack.TokenType.TokenType
import org.bson.BsonValue

class BsonReader(
    numberOfTokens: Int,
    tokenTypes:     Array[TokenType],
    strings:        Array[String],
    values:         Array[BsonValue]
) extends Reader {

  override var position: Int = -1

  // $COVERAGE-OFF$Never used for BSON but needed for Reader trait
  override def source: Array[Char] = ???

  override def tokenOffsetAt(position: Int): Int = ???

  override def tokenLengthAt(position: Int): Int = ???
  // $COVERAGE-ON$

  override def peek: TokenType = tokenTypes(position + 1)

  // $COVERAGE-OFF$Not used for BSON
  override def read(): TokenType = {
    position += 1
    tokenTypes(position)
  }
  // $COVERAGE-ON$

  override def read(expected: TokenType): Unit = {
    position += 1
    if (expected != tokenTypes(position)) {
      // $COVERAGE-OFF$Safety check--should never be possible
      throw new Exception("Wrong token type")
      // $COVERAGE-ON$
    }
  }

  override def readString(): String = {
    read(expected = TokenType.String)
    strings(position)
  }

  override def readInt(): Int = {
    read(expected = TokenType.Number)
    values(position).asInt32.intValue
  }

  override def readLong(): Long = {
    read(expected = TokenType.Number)
    values(position).asInt64.longValue
  }

  // This guy has to do type inference, as a Number could be: Byte, Double, Float, Integer, Long, or Short.
  override def readNumber(forJava: Boolean): Number = {
    read(expected = TokenType.Number)
    val value = values(position)
    if (value.isDouble) {
      value.asDouble.doubleValue
    } else if (value.isInt32) {
      value.asInt32.intValue
    } else if (value.isInt64) {
      value.asInt64.longValue
    } else {
      // $COVERAGE-OFF$Theoretically not possible
      throw new IllegalStateException(s"Cannot convert $value to a number")
      // $COVERAGE-ON$
    }
  }

  override def showError(): String = "SOMETHING WENT WRONG"

  // $COVERAGE-OFF$Not (currently) used for BSON
  override def tokenText: String = "TOKEN TEXT"

  override def captureValue(): Any = {
    // val startTok = position + 1
    // skipValue()
    // val endTok = Math.max(startTok, position)
    // new String(source.slice(tokenOffsets(startTok), tokenOffsets(endTok) + tokenLengths(endTok)))
    "nothing"
  }
  // $COVERAGE-ON$

}
