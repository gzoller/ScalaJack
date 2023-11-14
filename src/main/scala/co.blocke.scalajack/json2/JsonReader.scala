package co.blocke.scalajack
package json2

import scala.annotation._
import json.JsonParseError

case class JsonReader( js: CharSequence ):
    private var i = 0
    private val max = js.length

    inline def read(): Char = 
        if i < max then 
            i += 1
            history(i - 1)
        else BUFFER_EXCEEDED

    inline def readSkipWhitespace(): Char = 
        var c: Char = 0
        while { c = read(); isWhitespace(c) } do ()
        c

    inline private def history(p: Int): Char = js.charAt(p)

    inline def retract() = i -= 1

    inline private def isWhitespace(c: Char): Boolean =
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
                    case _                => throw JsonParseError(s"Invalid '\\${c2.toChar}' in string")
            case '"' => END_OF_STRING
            case BUFFER_EXCEEDED => throw new JsonParseError("Unexpected end of buffer")
            case c => c

    inline def nextHex4(): Char = 
        var i: Int      = 0
        var accum: Int  = 0
        while i < 4 do
            var c = read().toInt
            if (c == BUFFER_EXCEEDED)
                throw JsonParseError("unexpected EOB in string")
            c =
                if ('0' <= c && c <= '9') c - '0'
                else if ('A' <= c && c <= 'F') c - 'A' + 10
                else if ('a' <= c && c <= 'f') c - 'a' + 10
                else
                throw JsonParseError("Invalid hex character in string")
            accum = accum * 16 + c
            i += 1
        accum.toChar

        /*
        Learnings:

        1) Group implicit decoders into prioritized groups, favoring common/simple things like primitive types.  Eg. esoteric things like date/time would be lowest.
        2) Spell out specific collection types BEFORE a general "built" colleciton type--saves cycles doing the conversion
        3) Use @switch macro to tell Scala to use a lookup table, not if-then-else in match-case statements

        Unknown--as of yet unknown how classes are handled....

        TODO:
            [ ] Learn how this StringMatrix works
            [ ] Classes are *derived* decoders--magic!  Clearly some mix of reading fields (set in StringMatrix), using FieldKeyDecoder and map decoder, then
                constructed into a class....
                1. In JsonDecoder, implement the [K,V] decoder.
                2. Manually construct a class decoder using StringMatrix and supply field-specific decoders for each field
                3. Try it!
        */