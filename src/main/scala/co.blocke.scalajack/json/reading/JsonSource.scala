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
case class JsonSource(js: CharSequence, jsBytes: Array[Byte]):
  var i = 0
  private var expectFieldValue = false
  private[json] val max = js.length

  // Jsoniter ParseString machinery
  val ps = ParseString(jsBytes)
  val cbuf = new Array[Char](4048)

  def pos = i

  inline def here = js.charAt(i)

  private var c: Char = 0
  inline def readChar(): Char =
    if i < max then
      c = here
      i += 1
      c
    else BUFFER_EXCEEDED

  inline def readCharWS(): Char =
    var c: Char = 0
    while { c = readChar(); isWhitespace(c) && c != BUFFER_EXCEEDED } do ()
    c

  // inline def retract() = i -= 1

  // JSON definition of whitespace
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
  inline def readEscapedChar(): Char =
    readChar() match
      case '\\' =>
        val c2 = readChar()
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
      var c = readChar().toInt
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
  def expectFirstObjectField(fieldNameMatrix: StringMatrix): Int =
    readCharWS() match {
      case '{' =>
        readCharWS() match {
          case '"' =>
            var fi: Int = 0
            var bs: Long = fieldNameMatrix.initial
            var c: Int = -1
            while { c = readEscapedChar(); c != END_OF_STRING && c != BUFFER_EXCEEDED } do {
              bs = fieldNameMatrix.update(bs, fi, c)
              fi += 1
            }
            if readCharWS() != ':' then throw new JsonParseError(s"Expected ':' field separator", this)
            bs = fieldNameMatrix.exact(bs, fi)
            fieldNameMatrix.first(bs)
          case '}' => -2 // end-of-object (empty, not null)
          case c   => throw new JsonParseError(s"Expected object field name or '}' but found '$c'", this)
        }
      case 'n' =>
        readChars(JsonSource.ull, "null")
        -3 // null
      case c => throw new JsonParseError(s"Expected object start '{' but found '$c'", this)
    }

  def expectObjectField(fieldNameMatrix: StringMatrix): Int =
    readCharWS() match {
      case ',' =>
        readCharWS() match {
          case '"' =>
            var fi: Int = 0
            var bs: Long = fieldNameMatrix.initial
            var c: Int = -1
            while { c = readEscapedChar(); c != END_OF_STRING && c != BUFFER_EXCEEDED } do {
              bs = fieldNameMatrix.update(bs, fi, c)
              fi += 1
            }
            if readCharWS() != ':' then throw new JsonParseError(s"Expected ':' field separator", this)
            bs = fieldNameMatrix.exact(bs, fi)
            fieldNameMatrix.first(bs)
          case c => throw new JsonParseError(s"Expected object field name but found '$c'", this)
        }
      case '}' => -2 // end-of-object
      case c   => throw new JsonParseError(s"Expected ',' or '}' but found '$c'", this)
    }

  def expectArrayStart(): Boolean =
    readCharWS() match {
      case '[' =>
        i += 1
        true
      case 'n' =>
        readChars(JsonSource.ull, "null")
        false
      case c => throw new JsonParseError(s"Expected array start '[' but found '$c'", this)
    }

  /// ------------------- Continue refresh here.... >>>>>>>>

  // True if we got anything besides a ], False for ]
  def firstArrayElement(): Boolean =
    (readCharWS(): @switch) match
      case ']' => false
      case _ =>
        retract()
        true

  // True if we got a comma, and False for ]
  def nextArrayElement(): Boolean =
    (readCharWS(): @switch) match
      case ',' =>
        true
      case ']' =>
        false
      case c => throw JsonParseError(s"Expected ',' or ']' got '$c'", this)

  // True if we got a string (implies a retraction), False for }
  def firstField(): Boolean =
    (readCharWS(): @switch) match {
      case '"' => true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected string or '}' got '$c'", this)
    }

  // True if we got a comma, and False for }
  def nextField(): Boolean =
    (readCharWS(): @switch) match {
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
    while { c = readEscapedChar(); c != END_OF_STRING } do {
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
    }
    charWithWS(':')
    bs = fieldNameMatrix.exact(bs, fi)
    fieldNameMatrix.first(bs)

  // Value might be null!
  def expectString(): CharSequence =
    readCharWS() match {
      case '"' =>
        val parsedCount = ps.parseString(0, max - i, cbuf, i)
        i += parsedCount + 1
        new String(cbuf, 0, parsedCount)
      // retract()
      // parseString()
      case 'n' =>
        readChars(JsonSource.ull, "null")
        null
      case c => throw new JsonParseError(s"Expected a String value but got '$c'", this)
    }

  // private def parseString(): CharSequence =
  //   charWithWS('"')
  //   val sb = new FastStringBuilder(64)
  //   var c: Char = END_OF_STRING
  //   while
  //     c = readEscapedChar()
  //     c != END_OF_STRING
  //   do sb.append(c.toChar)
  //   sb.buffer

  inline def expectChar(): Char =
    expectString() match {
      case s if s.length == 1 => s.charAt(0)
      case s                  => throw new JsonParseError(s"Expected a Char value but got '$s'", this)
    }

  def expectBoolean(): Boolean =
    (readCharWS(): @switch) match
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
      if readChar() != expect(i) then throw JsonParseError(s"Expected $errMsg", this)
      i += 1

  private def checkNumber(): Unit =
    (readCharWS(): @switch) match
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' => ()
      case c                                                                     => throw JsonParseError(s"Expected a number, got $c", this)
    retract()

  inline def charWithWS(c: Char): Unit =
    val got = readCharWS()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'", this)

  def skipValue(): Unit =
    (readCharWS(): @switch) match {
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
    while isNumber(readChar()) do {}
    retract()
  }

  def skipString(): Unit =
    var i: Int = 0
    while { i = readCharWS(); i != -1 } do ()

  inline def retract() = i -= 1
