package co.blocke.scalajack.json

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JInt, JLong, JNull, JNumber, JObject, JString, JValue }

object JValueParser {

  private val NumberOfDigitsInMaxLongValue: Int = Long.MaxValue.toString.length

  def parse(source: String): JValue =
    parse(source.toCharArray)

  def parse(source: Array[Char]): JValue =
    parse(source, 0, source.length)

  def parse(source: Array[Char], offset: Int, length: Int): JValue = {
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
      while (isLiteralChar(source(position))) {
        position += 1
      }
      val endIndex = position
      new String(source, beginIndex, endIndex - beginIndex)
    }

    def readString(): String = {
      skipChar(expected = '"')
      val beginIndex = position
      while (source(position) != '"') {
        position += 1
      }
      val endIndex = position
      skipChar(expected = '"')
      new String(source, beginIndex, endIndex - beginIndex)
    }

    def readField(): (String, JValue) = {
      val key = readString()
      skipWhitespace()

      skipChar(expected = ':')
      skipWhitespace()

      val value = readJValue()
      (key, value)
    }

    def readJArray(): JArray = {
      skipChar(expected = '[')
      skipWhitespace()

      val elementsBuilder = List.newBuilder[JValue]

      source(position) match {
        case ']' =>
          skipChar(expected = ']')
          skipWhitespace()

        case _ =>
          val initialElement = readJValue()
          elementsBuilder += initialElement

          var readingElements = true
          while (readingElements) {
            source(position) match {
              case ',' =>
                skipChar(expected = ',')
                skipWhitespace()

                val element = readJValue()
                elementsBuilder += element

              case ']' =>
                skipChar(expected = ']')
                skipWhitespace()
                readingElements = false
            }
          }
      }

      JArray(elementsBuilder.result())
    }

    def readJNumber(): JValue with JNumber = {
      val beginIndex = position

      var containsDecimal = false
      var onlyContainsDigits = true

      var readingNumber = true
      while (position < maxPosition && readingNumber) {
        val char = source(position)
        if (char == '.') {
          containsDecimal = true
          onlyContainsDigits = false
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

      if (containsDecimal) {
        JDecimal(BigDecimal(new String(source, beginIndex, endIndex - beginIndex)))
      } else if (onlyContainsDigits) {
        if (length < NumberOfDigitsInMaxLongValue) {
          JLong(new String(source, beginIndex, length).toLong)
        } else if (length == NumberOfDigitsInMaxLongValue) {
          // On the border between JLong/JInt
          val string = new String(source, beginIndex, length)
          try {
            JLong(string.toLong)
          } catch {
            case _: NumberFormatException =>
              JInt(BigInt(string))
          }
        } else {
          JInt(BigInt(new String(source, beginIndex, length)))
        }
      } else {
        JLong(new String(source, beginIndex, length).toLong)
      }
    }

    def readJObject(): JObject = {
      skipChar(expected = '{')
      skipWhitespace()

      val fieldsBuilder = List.newBuilder[(String, JValue)]

      source(position) match {
        case '}' =>
          skipChar(expected = '}')
          skipWhitespace()

        case _ =>
          val initialField = readField()
          fieldsBuilder += initialField

          var readingFields = true
          while (readingFields) {
            source(position) match {
              case ',' =>
                skipChar(expected = ',')
                skipWhitespace()

                val field = readField()
                fieldsBuilder += field

              case '}' =>
                skipChar(expected = '}')
                skipWhitespace()
                readingFields = false
            }
          }
      }

      JObject(fieldsBuilder.result())
    }

    def readJString(): JString =
      JString(readString())

    def readJValue(): JValue = {
      source(position) match {
        case '{' =>
          readJObject()

        case '[' =>
          readJArray()

        case '"' =>
          readJString()

        case literalChar if isLiteralChar(literalChar) =>
          readLiteral() match {
            case "null"  => JNull
            case "false" => JBool.False
            case "true"  => JBool.True
          }

        case numberChar if isNumberChar(numberChar) =>
          readJNumber()
      }
    }

    skipWhitespace()
    readJValue()
  }

}
