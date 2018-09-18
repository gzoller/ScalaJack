package co.blocke.scalajack

import TokenType.TokenType
import scala.util.{ Try, Success, Failure }

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
      case TokenType.False =>
        false

      case TokenType.True =>
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
            case v if (v.isValidInt) => v.toInt
            case v                   => v
          }
          case _ => if (forJava) new java.math.BigInteger(tokenText) else BigInt(tokenText)
        }
    }
  }

  def readJsonValue[J]()(implicit ops: JsonOps[J]): J = {

    def readJsonArray(): J =
      JsonArray { appendElement =>
        read(expected = TokenType.BeginArray)

        while (hasMoreElements) {
          val elementJson = readJsonValue[J]()
          appendElement(elementJson)
        }

        read(expected = TokenType.EndArray)
      }

    def readJsonBoolean(): J =
      JsonBoolean(readBoolean())

    def readJsonNull(): J = {
      readNull()
      JsonNull()
    }

    def readJsonNumber(): J =
      readNumber() match {
        case x: java.lang.Integer     => JsonLong(x.longValue)
        case x: java.lang.Long        => JsonLong(x.longValue)
        case x: java.lang.Float       => JsonDouble(x.doubleValue)
        case x: java.lang.Double      => JsonDouble(x.doubleValue)
        case x: scala.math.BigInt     => JsonInt(x)
        case x: scala.math.BigDecimal => JsonDecimal(x)
      }

    def readJsonObject(): J =
      JsonObject { appendField =>
        read(expected = TokenType.BeginObject)

        while (hasMoreMembers) {
          val memberName = readString()
          val memberValue = readJsonValue[J]()
          appendField(memberName, memberValue)
        }

        read(expected = TokenType.EndObject)
      }

    def readJsonString(): J =
      JsonString(readString())

    peek match {
      case TokenType.BeginArray =>
        readJsonArray()

      case TokenType.BeginObject =>
        readJsonObject()

      case TokenType.False | TokenType.True =>
        readJsonBoolean()

      case TokenType.Number =>
        readJsonNumber()

      case TokenType.Null =>
        readJsonNull()

      case TokenType.String =>
        readJsonString()
    }
  }

  def readByte(): Byte = {
    read(expected = TokenType.Number)
    Try(tokenText.toByte) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def readShort(): Short = {
    read(expected = TokenType.Number)
    Try(tokenText.toShort) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def readInt(): Int = {
    read(expected = TokenType.Number)
    Try(tokenText.toInt) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def readLong(): Long = {
    read(expected = TokenType.Number)
    Try(tokenText.toLong) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def readFloat(): Float = {
    read(expected = TokenType.Number)
    Try(tokenText.toFloat) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def readDouble(): Double = {
    read(expected = TokenType.Number)
    Try(tokenText.toDouble) match {
      case Success(u) => u
      case Failure(u) => throw new NumberFormatException(u.getMessage + "\n" + showError())
    }
  }

  def skipValue(): Unit = {

    def skipToEndOfObject(): Unit = {
      var skipping = true
      while (skipping) {
        read() match {
          case TokenType.BeginObject =>
            skipToEndOfObject()

          case TokenType.EndObject =>
            skipping = false

          case TokenType.BeginArray =>
            skipToEndOfArray()

          case TokenType.EndArray =>
            throw new IllegalStateException(s"Encountered end-of-array token while inside an object\n${showError()}")

          case _ =>
        }
      }
    }

    def skipToEndOfArray(): Unit = {
      var skipping = true
      while (skipping) {
        read() match {
          case TokenType.BeginObject =>
            skipToEndOfObject()

          case TokenType.EndObject =>
            throw new IllegalStateException(s"Encountered end-of-object token while inside an array\n${showError()}")

          case TokenType.BeginArray =>
            skipToEndOfArray()

          case TokenType.EndArray =>
            skipping = false

          case _ =>
        }
      }
    }

    read() match {
      case TokenType.BeginObject =>
        skipToEndOfObject()

      case TokenType.EndObject =>
        throw new IllegalStateException(s"Encountered end-of-object token while not inside an object or array\n${showError()}")

      case TokenType.BeginArray =>
        skipToEndOfArray()

      case TokenType.EndArray =>
        throw new IllegalStateException(s"Encountered end-of-array token while not inside an object or array\n${showError()}")

      case _ =>
    }
  }

  def captureValue(): Any

  def tokenText: String

  def hasNext: Boolean = (peek != TokenType.End && peek != null) // differentiate end-of-parsing from end of object/array

  def hasMoreElements: Boolean = peek != TokenType.EndArray

  def hasMoreMembers: Boolean = peek != TokenType.EndObject

  def beginObject() = read(expected = TokenType.BeginObject)

  def endObject() = read(expected = TokenType.EndObject)

  def beginArray() = read(expected = TokenType.BeginArray)

  def endArray() = read(expected = TokenType.EndArray)

}
