package co.blocke.scalajack

object JsonParser {

  private val NumberOfDigitsInMaxLongValue: Int = Long.MaxValue.toString.length

  def parse[J](source: String)(implicit ops: JsonOps[J]): J =
    parse[J](source.toCharArray)

  def parse[J](source: Array[Char])(implicit ops: JsonOps[J]): J =
    parse[J](source, 0, source.length)

  def parse[J](source: Array[Char], offset: Int, length: Int)(implicit ops: JsonOps[J]): J = {
    var position = offset
    val maxPosition = offset + length

    @inline def isWhitespace(char: Char): Boolean =
      char match {
        case ' ' | '\r' | '\t' | '\n' => true
        case _                        => false
      }

    @inline def skipWhitespace(): Unit = {
      while (position < maxPosition && isWhitespace(source(position))) {
        position += 1
      }
    }

    @inline def skipChar(expected: Char): Unit = {
      val actual = source(position)
      if (actual == expected) {
        position += 1
      } else {
        throw new IllegalArgumentException(s"Skipped '$actual', not '$expected'")
      }
    }

    @inline def isLiteralChar(char: Char): Boolean =
      ('a' <= char && char < 'z') || ('A' <= char && char <= 'Z') || char == '_'

    @inline def isDigitChar(char: Char): Boolean =
      '0' <= char && char <= '9'

    @inline def isNumberChar(char: Char): Boolean =
      isDigitChar(char) || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

    def readLiteral(): String = {
      val beginIndex = position
      while (position < maxPosition && isLiteralChar(source(position))) {
        position += 1
      }
      val endIndex = position
      new String(source, beginIndex, endIndex - beginIndex)
    }

    def readString(): String = {
      skipChar(expected = '"')
      val beginIndex = position

      var segmentStart = position
      var stringBuilder: StringBuilder = null

      var reading = true
      while (reading) {
        val char = source(position)
        char match {
          case '"' =>
            reading = false

          case '\\' =>
            if (stringBuilder eq null) stringBuilder = new StringBuilder
            stringBuilder.appendAll(source, segmentStart, position - segmentStart)
            skipChar(expected = '\\')

            source(position) match {
              case '"' =>
                skipChar(expected = '"')
                stringBuilder.append('"')

              case '\\' =>
                skipChar(expected = '\\')
                stringBuilder.append('\\')

              case '/' =>
                skipChar(expected = '/')
                stringBuilder.append('/')

              case 'b' =>
                skipChar(expected = 'b')
                stringBuilder.append('\b')

              case 'f' =>
                skipChar(expected = 'f')
                stringBuilder.append('\f')

              case 'n' =>
                skipChar(expected = 'n')
                stringBuilder.append('\n')

              case 'r' =>
                skipChar(expected = 'r')
                stringBuilder.append('\r')

              case 't' =>
                skipChar(expected = 't')
                stringBuilder.append('\t')

              case 'u' =>
                skipChar(expected = 'u')

                val hex = new String(source, position, 4)
                stringBuilder.append(Integer.parseInt(hex, 16).toChar)
                position += 4
            }

            segmentStart = position

          case _ =>
            position += 1
        }
      }

      val endIndex = position
      skipChar(expected = '"')

      val string =
        if (stringBuilder eq null) {
          new String(source, beginIndex, endIndex - beginIndex)
        } else {
          stringBuilder.appendAll(source, segmentStart, endIndex - segmentStart)
          stringBuilder.result()
        }

      string
    }

    def readField(): (String, J) = {
      val startPosition = position
      val key = readJsonValue()
      val endPosition = position

      val keyString =
        key match {
          case JsonString(string) => string
          case _ =>
            // Non-standard JSON key
            new String(source, startPosition, endPosition - startPosition)
        }

      skipWhitespace()

      skipChar(expected = ':')
      skipWhitespace()

      val value = readJsonValue()
      (keyString, value)
    }

    def readJsonArray(): J =
      ops applyArray { appendElement =>
        skipChar(expected = '[')
        skipWhitespace()

        source(position) match {
          case ']' =>
            skipChar(expected = ']')
            skipWhitespace()

          case _ =>
            val initialElement = readJsonValue()
            appendElement(initialElement)

            var readingElements = true
            while (readingElements) {
              skipWhitespace()
              source(position) match {
                case ',' =>
                  skipChar(expected = ',')
                  skipWhitespace()

                  val element = readJsonValue()
                  appendElement(element)

                case ']' =>
                  skipChar(expected = ']')
                  skipWhitespace()
                  readingElements = false
              }
            }
        }
      }

    def readJsonNumber(): J = {
      val beginIndex = position

      var containsDecimal = false
      var onlyContainsDigits = true

      var readingNumber = true
      while (position < maxPosition && readingNumber) {
        val char = source(position)
        if (char == '.') {
          containsDecimal = true
          if (char != '-') {
            onlyContainsDigits = false
          }
          position += 1
        } else if (isDigitChar(char)) {
          position += 1
        } else if (isNumberChar(char)) {
          position += 1
          onlyContainsDigits = false
        } else {
          readingNumber = false
        }
      }

      val endIndex = position
      val length = endIndex - beginIndex

      val jsonNumber =
        if (containsDecimal) {
          val string = new String(source, beginIndex, endIndex - beginIndex)
          ops.applyDecimal(BigDecimal(string))
        } else if (onlyContainsDigits) {
          if (length < NumberOfDigitsInMaxLongValue) {
            ops.applyLong(new String(source, beginIndex, length).toLong)
          } else if (length == NumberOfDigitsInMaxLongValue) {
            // On the border between JLong/JInt
            val string = new String(source, beginIndex, length)
            try {
              ops.applyLong(string.toLong)
            } catch {
              case _: NumberFormatException =>
                ops.applyInt(BigInt(string))
            }
          } else {
            ops.applyInt(BigInt(new String(source, beginIndex, length)))
          }
        } else {
          val string = new String(source, beginIndex, length)
          try {
            ops.applyLong(string.toLong)
          } catch {
            case _: NumberFormatException =>
              ops.applyInt(BigInt(string))
          }
        }

      jsonNumber
    }

    def readJsonObject(): J =
      ops applyObject { appendField =>
        skipChar(expected = '{')
        skipWhitespace()

        source(position) match {
          case '}' =>
            skipChar(expected = '}')
            skipWhitespace()

          case _ =>
            val (initialFieldName, initialFieldValue) = readField()
            appendField(initialFieldName, initialFieldValue)

            var readingFields = true
            while (readingFields) {
              skipWhitespace()
              source(position) match {
                case ',' =>
                  skipChar(expected = ',')
                  skipWhitespace()

                  val (fieldName, fieldValue) = readField()
                  appendField(fieldName, fieldValue)

                case '}' =>
                  skipChar(expected = '}')
                  skipWhitespace()
                  readingFields = false
              }
            }
        }
      }

    def readJsonString(): J =
      ops.applyString(readString())

    def readJsonValue(): J = {
      source(position) match {
        case '{' =>
          readJsonObject()

        case '[' =>
          readJsonArray()

        case '"' =>
          readJsonString()

        case literalChar if isLiteralChar(literalChar) =>
          readLiteral() match {
            case "null"  => ops.applyNull()
            case "false" => ops.applyBoolean(false)
            case "true"  => ops.applyBoolean(true)
          }

        case numberChar if isNumberChar(numberChar) =>
          readJsonNumber()
      }
    }

    skipWhitespace()
    readJsonValue()
  }

}
