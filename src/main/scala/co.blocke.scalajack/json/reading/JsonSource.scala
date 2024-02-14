package co.blocke.scalajack
package json
package reading

import scala.annotation.*

object JsonSource:
  protected val ull: Array[Char] = "ull".toCharArray
  protected val alse: Array[Char] = "alse".toCharArray
  protected val rue: Array[Char] = "rue".toCharArray

// ZIO-Json defines a series of different Readers.  Not exactly sure why--maybe to support different
// modes (streaming, ...)? At least for now we only need one, so merged key bits of Readers into one.
case class JsonSource(js: CharSequence):
  private var i = 0
  private var expectFieldValue = false
  private[json] val max = js.length

  def pos = i

  inline def here = js.charAt(i)

  inline def read(): Char =
    if i < max then
      val c = history(i)
      i += 1
      c
    else BUFFER_EXCEEDED

  inline def readSkipWhitespace(): Char =
    var c: Char = 0
    while { c = read(); isWhitespace(c) } do ()
    c

  private inline def history(p: Int): Char = js.charAt(p)

  inline def retract() = i -= 1

  private inline def isWhitespace(c: Char): Boolean =
    (c: @switch) match {
      case ' '  => true
      case '\r' => true
      case '\n' => true
      case '\t' => true
      case _    => false
    }

  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' => true
      case _                                                                                       => false

  // Read, transforming escaped chars and stopping when we hit '"'
  inline def readEscapedString(): Char =
    read() match
      case '\\' =>
        val c2 = read()
        (c2: @switch) match
          case '"' | '\\' | '/' => c2
          case 'b'              => '\b'
          case 'f'              => '\f'
          case 'n'              => '\n'
          case 'r'              => '\r'
          case 't'              => '\t'
          case 'u'              => nextHex4()
          case _                => throw JsonParseError(s"Invalid '\\${c2.toChar}' in string", this)
      case '"'             => END_OF_STRING
      case BUFFER_EXCEEDED => throw new JsonParseError("Unexpected end of buffer", this)
      case c               => c

  inline def nextHex4(): Char =
    var i: Int = 0
    var accum: Int = 0
    while i < 4 do
      var c = read().toInt
      if c == BUFFER_EXCEEDED then throw JsonParseError("Unexpected EOB in string", this)
      c =
        if '0' <= c && c <= '9' then c - '0'
        else if 'A' <= c && c <= 'F' then c - 'A' + 10
        else if 'a' <= c && c <= 'f' then c - 'a' + 10
        else throw JsonParseError("Invalid hex character in string", this)
      accum = accum * 16 + c
      i += 1
    accum.toChar

//-------

  // returns false if 'null' found
  def expectObjectStart(): Boolean =
    readSkipWhitespace() match {
      case '{' =>
        true
      case 'n' =>
        readChars(JsonSource.ull, "null")
        false
      case c => throw new JsonParseError(s"Expected object start '{' but found '$c'", this)
    }

  def expectArrayStart(): Boolean =
    readSkipWhitespace() match {
      case '[' =>
        true
      case 'n' =>
        readChars(JsonSource.ull, "null")
        false
      case c => throw new JsonParseError(s"Expected array start '[' but found '$c'", this)
    }

  // True if we got anything besides a ], False for ]
  def firstArrayElement(): Boolean =
    (readSkipWhitespace(): @switch) match
      case ']' => false
      case _ =>
        retract()
        true

  // True if we got a comma, and False for ]
  def nextArrayElement(): Boolean =
    (readSkipWhitespace(): @switch) match
      case ',' =>
        true
      case ']' =>
        false
      case c => throw JsonParseError(s"Expected ',' or ']' got '$c'", this)

  // True if we got a string (implies a retraction), False for }
  def firstField(): Boolean =
    (readSkipWhitespace(): @switch) match {
      case '"' => true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected string or '}' got '$c'", this)
    }

  // True if we got a comma, and False for }
  def nextField(): Boolean =
    (readSkipWhitespace(): @switch) match {
      case ',' =>
        expectFieldValue = false
        true
      // case '}' if !expectFieldValue =>
      //   false
      case '}' =>
        false
      // throw JsonParseError("Expected field value but got '}' instead.", this)
      case c =>
        throw JsonParseError(s"expected ',' or '}' got '$c'", this)
    }

  inline def expectFieldName(fieldNameMatrix: StringMatrix): Int =
    charWithWS('"')
    expectFieldValue = true
    var fi: Int = 0
    var bs: Long = fieldNameMatrix.initial
    var c: Int = -1
    while { c = readEscapedString(); c != END_OF_STRING } do {
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
    }
    charWithWS(':')
    bs = fieldNameMatrix.exact(bs, fi)
    fieldNameMatrix.first(bs)

  // Value might be null!
  def expectString(): CharSequence =
    readSkipWhitespace() match {
      case '"' =>
        retract()
        parseString()
      case 'n' =>
        readChars(JsonSource.ull, "null")
        null
      case c => throw new JsonParseError(s"Expected a String value but got '$c'", this)
    }

  private def parseString(): CharSequence =
    charWithWS('"')
    val sb = new FastStringBuilder(64)
    while true do
      val c = readEscapedString()
      if c == END_OF_STRING then return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    throw JsonParseError("Invalid string value detected", this)

  inline def expectChar(): Char =
    expectString() match {
      case s if s.length == 1 => s.charAt(0)
      case s                  => throw new JsonParseError(s"Expected a Char value but got '$s'", this)
    }

  def expectBoolean(): Boolean =
    (readSkipWhitespace(): @switch) match
      case 't' =>
        readChars(JsonSource.rue, "true")
        true
      case 'f' =>
        readChars(JsonSource.alse, "false")
        false
      case c => throw JsonParseError(s"Expected 'true' or 'false' got '$c'", this)

  def expectInt(): Int =
    checkNumber()
    try {
      val intRead = UnsafeNumbers.int_(this, false)
      retract()
      intRead
    } catch {
      case UnsafeNumbers.UnsafeNumber => throw JsonParseError("Expected an Int", this)
    }

  private inline def readChars(
      expect: Array[Char],
      errMsg: String
  ): Unit =
    var i: Int = 0
    while i < expect.length do
      if read() != expect(i) then throw JsonParseError(s"Expected $errMsg", this)
      i += 1

  private def checkNumber(): Unit =
    (readSkipWhitespace(): @switch) match
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' => ()
      case c                                                                     => throw JsonParseError(s"Expected a number, got $c", this)
    retract()

  inline def charWithWS(c: Char): Unit =
    val got = readSkipWhitespace()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'", this)

  def skipValue(): Unit =
    (readSkipWhitespace(): @switch) match {
      case 'n' => readChars(JsonSource.ull, "null")
      case 'f' => readChars(JsonSource.alse, "false")
      case 't' => readChars(JsonSource.rue, "true")
      case '{' =>
        if firstField() then {
          while {
            {
              charWithWS('"')
              skipString()
              charWithWS(':')
              skipValue()
            }; nextField()
          } do ()
        }
      case '[' =>
        if firstArrayElement() then {
          while { skipValue(); nextArrayElement() } do ()
        }
      case '"' =>
        skipString()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' =>
        skipNumber()
      case c => throw JsonParseError(s"Unexpected '$c'", this)
    }

  def skipNumber(): Unit = {
    while isNumber(read()) do {}
    retract()
  }

  def skipString(): Unit =
    var i: Int = 0
    while { i = readEscapedString(); i != -1 } do ()
