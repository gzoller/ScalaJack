package co.blocke.scalajack
package json

import scala.annotation.*

// ZIO-Json defines a series of different Readers.  Not exactly sure why--maybe to support different
// modes (streaming, ...)? At least for now we only need one, so merged key bits of Readers into one.
case class JsonSource(js: CharSequence):
  private var i = 0
  private[json] val max = js.length

  def pos = i

  inline def read(): Char =
    if i < max then
      i += 1
      history(i - 1)
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
