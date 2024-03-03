package co.blocke.scalajack
package json
package reading

import scala.annotation.{switch, tailrec}

object JsonSource:
  protected val ull: Array[Char] = "ull".toCharArray
  protected val alse: Array[Char] = "alse".toCharArray
  protected val rue: Array[Char] = "rue".toCharArray
  protected val falseBytes = 'f' | 'a' << 8 | 'l' << 16 | 's' << 24 | 'e' << 32
  protected val trueBytes = 't' | 'r' << 8 | 'u' << 16 | 'e' << 24

// ZIO-Json defines a series of different Readers.  Not exactly sure why--maybe to support different
// modes (streaming, ...)? At least for now we only need one, so merged key bits of Readers into one.
case class JsonSource(js: CharSequence):
  var i = 0
  private var expectFieldValue = false
  private[json] val max = js.length

  def pos = i

  // inline def here = js(i).toChar
  inline def here = js.charAt(i)

  inline def revert = i -= 1

  private var c: Char = 0
  inline def readChar(): Char =
    if i < max then
      c = here
      i += 1
      c
    else BUFFER_EXCEEDED

  // JSON definition of whitespace
  private inline def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\n' || (c | 0x4) == '\r' || c == '\t'

  inline def readCharWS(): Char =
    while isWhitespace(here) && i < max do i += 1
    readChar()

  inline def skipWS(): Unit =
    while isWhitespace(here) && i < max do i += 1
    if i == max then throw new JsonParseError("Unexpected end of buffer", this)

  inline def retract() = i -= 1

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

  inline def expectObjectOrNull(): Boolean = // false => null
    readCharWS() match {
      case '{' => true
      case 'n' =>
        readChars(JsonSource.ull, "null")
        false
      case _ => throw new JsonParseError(s"Expected object start '{' or null", this)
    }

  inline def expectObjectField(): CharSequence =
    val endI = parseString(i)
    val fname = js.subSequence(i, endI)
    i = endI + 1
    if readCharWS() != ':' then throw new JsonParseError(s"Expected ':' separator token", this)
    fname

  def expectArray[E](f: () => E): scala.collection.mutable.ListBuffer[E] =
    (readCharWS(): @switch) match
      case '[' =>
        val seq = scala.collection.mutable.ListBuffer.empty[E]
        skipWS()
        while i < max && here != ']' do
          seq.addOne(f())
          readCharWS() match {
            case ']' => retract()
            case ',' =>
            case c   => throw JsonParseError(s"Expected ',' or ']' got '$c'", this)
          }
        i += 1
        seq
      case 'n' =>
        readChars(JsonSource.ull, "null")
        null

  // ---------------
  // DEPRECATED !!!
  // ---------------
  // True if we got anything besides a ], False for ]
  def firstArrayElement(): Boolean =
    (readCharWS(): @switch) match
      case ']' => false
      case _ =>
        retract()
        true

  // ---------------
  // DEPRECATED !!!
  // ---------------
  // True if we got a comma, and False for ]
  def nextArrayElement(): Boolean =
    (readCharWS(): @switch) match
      case ',' =>
        true
      case ']' =>
        false
      case c => throw JsonParseError(s"Expected ',' or ']' got '$c'", this)

  // ---------------
  // DEPRECATED !!!
  // ---------------
  // True if we got a string (implies a retraction), False for }
  def firstField(): Boolean =
    (readCharWS(): @switch) match {
      case '"' => true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected string or '}' got '$c'", this)
    }

  // ---------------
  // DEPRECATED !!!
  // ---------------
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

  // Value might be null!
  // expectString() will look for leading '"'.  parseString() presumes the '"' has already been consumed.
  inline def expectString(): CharSequence =
    readCharWS() match {
      case '"' =>
        val endI = parseString(i)
        val str = js.subSequence(i, endI)
        i = endI + 1
        str
      case 'n' =>
        readChars(JsonSource.ull, "null")
        null
      case c => throw new JsonParseError(s"Expected a String value but got '$c'", this)
    }

  @tailrec
  final def parseString(pos: Int): Int =
    if pos + 3 < max then { // Based on SWAR routine of JSON string parsing: https://github.com/sirthias/borer/blob/fde9d1ce674d151b0fee1dd0c2565020c3f6633a/core/src/main/scala/io/bullet/borer/json/JsonParser.scala#L456
      val bs = (js.charAt(pos)) | (js.charAt(pos + 1) << 8) | (js.charAt(pos + 2) << 16) | js.charAt(pos + 3) << 24
      val mask = ((bs - 0x20202020 ^ 0x3c3c3c3c) - 0x1010101 | (bs ^ 0x5d5d5d5d) + 0x1010101) & 0x80808080
      if mask != 0 then {
        val offset = java.lang.Integer.numberOfTrailingZeros(mask) >> 3
        if (bs >> (offset << 3)).toByte == '"' then pos + offset
        else throw new Exception("special chars found 1") // else parseEncodedString(i + offset, charBuf.length - 1, charBuf, pos + offset)
      } else parseString(pos + 4)
    } else if pos < max then {
      val b = js.charAt(pos)
      if b == '"' then pos
      else if (b - 0x20 ^ 0x3c) <= 0 then throw new Exception("special chars found 2") // parseEncodedString(i, charBuf.length - 1, charBuf, pos)
      else parseString(pos + 1)
    } else throw new Exception("Buffer exceeded--string too long")

  inline def expectChar(): Char =
    expectString() match {
      case s if s.length == 1 => s.charAt(0)
      case s                  => throw new JsonParseError(s"Expected a Char value but got '$s'", this)
    }

  def expectBoolean(): Boolean =
    while isWhitespace(here) do i += 1
    val bs = (js.charAt(pos)) | (js.charAt(pos + 1) << 8) | (js.charAt(pos + 2) << 16) | js.charAt(pos + 3) << 24
    if (bs ^ JsonSource.trueBytes) == 0 then
      i += 4
      true
    else if ((bs | js.charAt(pos + 4)) ^ JsonSource.falseBytes) == 0 then
      i += 5
      false
    else throw JsonParseError(s"Expected 'true' or 'false'", this)

  def expectInt(): Int =
    if { skipWS(); !isNumber(here) } then throw JsonParseError(s"Expected a number, got $c", this)
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
