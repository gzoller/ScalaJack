package co.blocke.scalajack
package json
package reading

import scala.annotation.*

object JsonSource2:
  protected val ull: Array[Char] = "ull".toCharArray
  protected val _null: Array[Char] = "null".toCharArray
  protected val alse: Array[Char] = "alse".toCharArray
  protected val rue: Array[Char] = "rue".toCharArray

case class JsonSource2(js: CharSequence):
  private var i = 0   // The great, omnipresent index
  private[json] val max = js.length  // don't overrun this, or else...

  inline def here: Char = js.charAt(i)
  inline def pos = i

  // Parse philosophy: After parsing a character, always leave index (i) ready to read the next
  // character

  private inline def getCharWS: Char =
    while i < max && here.isWhitespace do i += 1
    if i == max then BUFFER_EXCEEDED
    else here

  private inline def expectChars(
      expect: Array[Char],
      errMsg: String
  ): Unit =
    if i + expect.length >= max then throw JsonParseError2("(a) Tried to read past end of JSON buffer", this)
    var j = 0
    while j < expect.length && here == expect(j) do { i += 1; j += 1 }
    if j == expect.length then ()
    else throw JsonParseError2(s"(a) Expected $errMsg", this)

  // -- Parse Objects

  inline def expectObject: Boolean =  // returns false if null
    getCharWS match {
        case '{' => 
            i += 1
            true // true -> start of object found
        case 'n' => 
            i += 1
            expectChars(JsonSource2.ull, "'{' or null")
            false // false -> null found
        case BUFFER_EXCEEDED => throw JsonParseError2("(1) Tried to read past end of JSON buffer", this)
        case _ => 
            throw JsonParseError2("(1) Expected '{' or null", this)
    }

  def expectFieldIndex(fieldNameMatrix: StringMatrix): Int =
    (getCharWS: @switch) match {
        case '"' => 
          var fi: Int = 0
          var bs: Long = fieldNameMatrix.initial
          var c: Int = -1
          i += 1
          while i < max && here != '"' do 
            bs = fieldNameMatrix.update(bs, fi, here)
            i += 1
            fi += 1
          bs = fieldNameMatrix.exact(bs, fi)
          if i == max then throw JsonParseError2("(2) Tried to read past end of JSON buffer", this)
          else
            i += 1
            (getCharWS: @switch) match {
                case ':' => 
                  i += 1
                  fieldNameMatrix.first(bs)
                case BUFFER_EXCEEDED => throw JsonParseError2("(2) Tried to read past end of JSON buffer", this)
                case _ => throw JsonParseError2(s"(2) Expected ':' but got '$here'", this)
            }
        case BUFFER_EXCEEDED => throw JsonParseError2("(2) Tried to read past end of JSON buffer", this)
        case _ => throw JsonParseError2(s"(2) Expected start of field name (string) '\"' but got '$here'", this)
    }

  inline def hasFields: Boolean = 
    (getCharWS: @switch) match {
      case '}' => 
        i += 1
        false
      case _ => true
    }

  def nextField: Boolean = // true means there is another field.  false means '}' -- end of object
    (getCharWS: @switch) match {
      case '}' => 
        i += 1
        false
      case ',' => 
        i += 1
        true
      case _ =>
        throw JsonParseError2(s"(3) Malformed JSON. Expected ',' field separator", this)
    }

  // -- Parse Array

  inline def expectArray: Boolean =  // returns false if null
    getCharWS match {
        case '[' => 
            i += 1
            true // true -> start of object found
        case 'n' => 
            i += 1
            expectChars(JsonSource2.ull, "'[' or null")
            false // false -> null found
        case BUFFER_EXCEEDED => throw JsonParseError2("(4) Tried to read past end of JSON buffer", this)
        case _ => 
            throw JsonParseError2("(4) Expected '[' or null", this)
    }

  inline def hasElements: Boolean = 
    (getCharWS: @switch) match {
      case ']' => 
        i += 1
        false
      case _ => true
    }

  def nextElement: Boolean = // true means there is another field.  false means '}' -- end of object
    (getCharWS: @switch) match {
      case ']' => 
        i += 1
        false
      case ',' => 
        i += 1
        true
      case _ =>
        throw JsonParseError2(s"(5) Malformed JSON. Expected ',' field separator", this)
    }

  // -- Parse JSON Simple Data Types

  def expectBoolean: Boolean =
    getCharWS match {
      case 't' =>
        i += 1
        expectChars(JsonSource2.rue, "true")
        true
      case 'f' =>
        i += 1
        expectChars(JsonSource2.alse, "false")
        false
      case BUFFER_EXCEEDED => throw JsonParseError2("(6) Tried to read past end of JSON buffer", this)
      case _ => throw JsonParseError2(s"(6) Expected boolean value", this)
    }