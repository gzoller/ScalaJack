package co.blocke.scalajack

import TokenType.TokenType
import scala.util.{ Try, Success }
import java.lang.NumberFormatException

trait Reader {

  var position: Int

  def source: Array[Char]

  def tokenOffset: Int = tokenOffsetAt(position)

  def tokenOffsetAt(position: Int): Int

  def tokenLength: Int = tokenLengthAt(position)

  def tokenLengthAt(position: Int): Int

  def showError(): String

  def peek: TokenType

  def read(): TokenType

  def read(expected: TokenType): Unit

  def readString(): String

  def readNull[T]()(implicit ev: Null <:< T): T = {
    read(expected = TokenType.Null)
    ev(null)
  }

  def readBoolean(): Boolean = {
    val bool = peek match {
      case TokenType.False ⇒
        false

      case TokenType.True ⇒
        true
    }
    position += 1
    bool
  }

  // This guy has to do type inference, as a Number could be: Byte, Double, Float, Integer, Long, or Short.
  // For reads that means the safest thing to do is always read the largest Number: Double or Long.
  // We need to decide if the text is an integer value or not to choose.
  def readNumber(forJava: Boolean = false): java.lang.Number = {
    read(expected = TokenType.Number)
    val tokenText = this.tokenText
    tokenText match {
      case floatVal if (tokenText.contains('.')) =>
        BigDecimal(tokenText) match {
          case f if (f.isDecimalFloat)  => f.toFloat
          case f if (f.isDecimalDouble) => f.toDouble
          case f                        => if (forJava) f.bigDecimal else f
        }
      case intVal =>
        Try(tokenText.toLong) match {
          case Success(v) => v match {
            case v if (v.isValidByte)  => v.toByte
            case v if (v.isValidShort) => v.toShort
            case v if (v.isValidInt)   => v.toInt
            case v                     => v
          }
          case _ => if (forJava) new java.math.BigInteger(tokenText) else BigInt(tokenText)
        }
    }
  }

  def readByte(): Byte = {
    read(expected = TokenType.Number)
    try {
      tokenText.toByte
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def readShort(): Short = {
    read(expected = TokenType.Number)
    try {
      tokenText.toShort
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def readInt(): Int = {
    read(expected = TokenType.Number)
    try {
      tokenText.toInt
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def readLong(): Long = {
    read(expected = TokenType.Number)
    try {
      tokenText.toLong
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def readFloat(): Float = {
    read(expected = TokenType.Number)
    try {
      tokenText.toFloat
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def readDouble(): Double = {
    read(expected = TokenType.Number)
    try {
      tokenText.toDouble
    } catch {
      case nfe: NumberFormatException =>
        throw new NumberFormatException(nfe.getMessage + "\n" + showError())
    }
  }

  def skipValue(): Unit = {
    peek match {
      case TokenType.BeginObject ⇒ skipOver(TokenType.BeginObject, TokenType.EndObject)

      case TokenType.BeginArray  ⇒ skipOver(TokenType.BeginArray, TokenType.EndArray)

      case _ ⇒
        position += 1
    }
  }

  private def skipOver(beginToken: TokenType.Value, endToken: TokenType.Value): Unit = {
    var depth = 0
    while (depth > 0 || peek != endToken) {
      println(s"Peek ${depth}:${position}: ${peek}")
      position += 1
      peek match {
        case `beginToken` ⇒
          depth += 1
        case `endToken` if (depth > 0) ⇒
          depth -= 1
          position += 1
        case _ ⇒
      }
    }
    position += 1
  }

  def tokenText: String

  def hasNext: Boolean = peek != TokenType.End // differentiate end-of-parsing from end of object/array

  def hasMoreElements: Boolean = peek != TokenType.EndArray

  def hasMoreMembers: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
