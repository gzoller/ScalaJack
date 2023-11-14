package co.blocke.scalajack
package json2

import json.JsonParseError
import scala.annotation._

object JsonParser:

  private val ull:  Array[Char] = "ull".toCharArray
  private val alse: Array[Char] = "alse".toCharArray
  private val rue:  Array[Char] = "rue".toCharArray

  def parseBoolean(gen: JsonReader): Boolean =
    (gen.readSkipWhitespace(): @switch) match {
      case 't' =>
        readChars(gen, rue, "true")
        true
      case 'f' =>
        readChars(gen, alse, "false")
        false
      case c =>
        throw JsonParseError(s"Expected true or false value")
    }

  def parseInt(gen: JsonReader): Int = {
    checkNumber(gen)
    try {
      val i = UnsafeNumbers.int_(gen, false)
      gen.retract()
      i
    } catch {
      case UnsafeNumbers.UnsafeNumber => throw JsonParseError("Expected an Int")
    }
  }

  def parseString(gen: JsonReader): CharSequence = 
    charWithWS(gen, '"')
    val sb = new FastStringBuilder(64)
    while true do
      val c = gen.readEscapedString()
      if (c == END_OF_STRING)
        return sb.buffer // mutable thing escapes, but cannot be changed
      sb.append(c.toChar)
    throw JsonParseError("Invalid string value detected")

  // Returns index of field name read in, or -1 if not found
  def parseField(gen: JsonReader, fieldMatrix: StringMatrix): Int =
    val f = enumeration(gen, fieldMatrix)
    charWithWS(gen, ':')
    f

  // True if we got anything besides a ], False for ]
  def firstArrayElement(in: JsonReader): Boolean =
    (in.readSkipWhitespace(): @switch) match
      case ']' => false
      case _   =>
        in.retract()
        true

  def nextArrayElement(in: JsonReader): Boolean =
    (in.readSkipWhitespace(): @switch) match 
      case ',' => true
      case ']' => false
      case c   => throw JsonParseError(s"expected ',' or ']' got '$c'")

  // True if we got a string (implies a retraction), False for }
  def firstField(in: JsonReader): Boolean =
    (in.readSkipWhitespace(): @switch) match {
      case '"' => true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected string or '}' got '$c'")
    }

  // True if we got a comma, and False for }
  def nextField(in: JsonReader): Boolean =
    (in.readSkipWhitespace(): @switch) match {
      case ',' => 
        charWithWS(in, '"')
        true
      case '}' => false
      case c =>
        throw JsonParseError(s"expected ',' or '}' got '$c'")
    }

  def skipValue(in: JsonReader): Unit =
    (in.readSkipWhitespace(): @switch) match {
      case 'n' => readChars(in, ull, "null")
      case 'f' => readChars(in, alse, "false")
      case 't' => readChars(in, rue, "true")
      case '{' =>
        if (firstField(in)) {
          while ({
            {
              char(in, '"')
              skipString(in)
              char(in, ':')
              skipValue(in)
            }; nextField(in)
          }) ()
        }
      case '[' =>
        if (firstArrayElement(in)) {
          while ({ skipValue(in); nextArrayElement(in) }) ()
        }
      case '"' =>
        skipString(in)
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' =>
        skipNumber(in)
      case c => throw JsonParseError(s"Unexpected '$c'")
    }

  def skipNumber(in: JsonReader): Unit = {
    while (isNumber(in.read())) {}
    in.retract()
  }

  def skipString(in: JsonReader): Unit =
    var i: Int = 0
    while ({ i = in.readEscapedString(); i != -1 }) ()

  private def checkNumber(gen: JsonReader): Unit =
    (gen.readSkipWhitespace(): @switch) match 
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' => ()
      case c => throw JsonParseError(s"Expected a number, got $c")
    gen.retract()

  @inline private[this] def isNumber(c: Char): Boolean =
    (c: @switch) match
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' => true
      case _ => false

  private inline def readChars(
    gen: JsonReader,
    expect: Array[Char],
    errMsg: String
  ): Unit = 
    var i: Int = 0
    while i < expect.length do
      if gen.read() != expect(i) then throw JsonParseError(s"Expected ${errMsg}")
      i += 1

  @inline def charWithWS(gen: JsonReader, c: Char): Unit =
    val got = gen.readSkipWhitespace()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'")

  @inline def char(gen: JsonReader, c: Char): Unit =
    val got = gen.read()
    if got != c then throw JsonParseError(s"Expected '$c' got '$got'")

  def enumeration(
    gen: JsonReader,
    matrix: StringMatrix
  ): Int = {
    var i: Int   = 0
    var bs: Long = matrix.initial
    var c: Int   = -1
    while ({ c = gen.readEscapedString(); c != END_OF_STRING }) {
      bs = matrix.update(bs, i, c)
      i += 1
    }
    bs = matrix.exact(bs, i)
    matrix.first(bs)
  }