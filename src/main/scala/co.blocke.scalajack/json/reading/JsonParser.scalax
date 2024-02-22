package co.blocke.scalajack
package json
package reading

import scala.annotation.*

object JsonParser:

  private val ull: Array[Char] = "ull".toCharArray
  private val alse: Array[Char] = "alse".toCharArray
  private val rue: Array[Char] = "rue".toCharArray

  def parseBoolean(in: JsonSource): Boolean =
    (in.readSkipWhitespace(): @switch) match {
      case 't' =>
        readChars(in, rue, "true")
        true
      case 'f' =>
        readChars(in, alse, "false")
        false
      case c =>
        throw JsonParseError(s"Expected true or false value", in)
    }

  def parseInt(in: JsonSource): Int = {
    checkNumber(in)
    try {
      val i = UnsafeNumbers.int_(in, false)
      in.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => throw JsonParseError("Expected an Int", in)
    }
  }

  def parseString(in: JsonSource): CharSequence =
    charWithWS(in, '"')
    val sb = new FastStringBuilder(64)
    while true do
      val c = in.readEscapedString()
      if c == END_OF_STRING then return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    throw JsonParseError("Invalid string value detected", in)

  // Returns index of field name read in, or -1 if not found
  def parseField(in: JsonSource, fieldMatrix: StringMatrix): Int =
    val f = enumeration(in, fieldMatrix)
    charWithWS(in, ':')
    f

  // True if we got anything besides a ], False for ]
  def firstArrayElement(in: JsonSource): Boolean =
    (in.readSkipWhitespace(): @switch) match
      case ']' => false
      case _ =>
        in.retract()
        true

  def nextArrayElement(in: JsonSource): Boolean =
    (in.readSkipWhitespace(): @switch) match
      case ',' => true
      case ']' => false
      case c   => throw JsonParseError(s"expected ',' or ']' got '$c'", in)

  // True if we got a string (implies a retraction), False for }
  def firstField(in: JsonSource): Boolean =
    (in.readSkipWhitespace(): @switch) match {
      case '"' => true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected string or '}' got '$c'", in)
    }

  // True if we got a comma, and False for }
  def nextField(in: JsonSource): Boolean =
    (in.readSkipWhitespace(): @switch) match {
      case ',' =>
        charWithWS(in, '"')
        true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected ',' or '}' got '$c'", in)
    }

  def skipValue(in: JsonSource): Unit =
    (in.readSkipWhitespace(): @switch) match {
      case 'n' => readChars(in, ull, "null")
      case 'f' => readChars(in, alse, "false")
      case 't' => readChars(in, rue, "true")
      case '{' =>
        if firstField(in) then {
          while {
            {
              char(in, '"')
              skipString(in)
              char(in, ':')
              skipValue(in)
            }; nextField(in)
          } do ()
        }
      case '[' =>
        if firstArrayElement(in) then {
          while { skipValue(in); nextArrayElement(in) } do ()
        }
      case '"' =>
        skipString(in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' =>
        skipNumber(in)
      case c => throw JsonParseError(s"Unexpected '$c'", in)
    }

  def skipNumber(in: JsonSource): Unit = {
    while isNumber(in.read()) do {}
    in.retract()
  }

  def skipString(in: JsonSource): Unit =
    var i: Int = 0
    while { i = in.readEscapedString(); i != -1 } do ()

  private def checkNumber(in: JsonSource): Unit =
    (in.readSkipWhitespace(): @switch) match
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' => ()
      case c                                                                     => throw JsonParseError(s"Expected a number, got $c", in)
    in.retract()

  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' => true
      case _                                                                                       => false

  private inline def readChars(
      in: JsonSource,
      expect: Array[Char],
      errMsg: String
  ): Unit =
    var i: Int = 0
    while i < expect.length do
      if in.read() != expect(i) then throw JsonParseError(s"Expected $errMsg", in)
      i += 1

  @inline def charWithWS(in: JsonSource, c: Char): Unit =
    val got = in.readSkipWhitespace()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'", in)

  @inline def char(in: JsonSource, c: Char): Unit =
    val got = in.read()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'", in)

  def enumeration(
      gen: JsonSource,
      matrix: StringMatrix
  ): Int = {
    var i: Int = 0
    var bs: Long = matrix.initial
    var c: Int = -1
    while { c = gen.readEscapedString(); c != END_OF_STRING } do {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }
