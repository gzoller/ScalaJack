package co.blocke.scalajack
package json
package exp

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import java.io.InputStream
import java.nio.charset.StandardCharsets.UTF_8
import scala.specialized

/*
Bottom Line:
  Fancy string reading, ByteBuffer handling, etc. did NOT have a material effect on speed.
  The SJ way was either equal to, or faster than, the JsonReader approach for reading strings!
 */

class JsonReader private[json] (
    private[this] var buf: Array[Byte] = new Array[Byte](32768),
    private[this] var head: Int = 0,
    private[this] var tail: Int = 0,
    private[this] var mark: Int = -1,
    private[this] var charBuf: Array[Char] = new Array[Char](4096),
    private[this] var bbuf: ByteBuffer = null,
    private[this] var in: InputStream = null,
    private[this] var totalRead: Long = 0
):

  private[json] def read(s: String): String = {
    val currBuf = this.buf
    try {
      this.buf = s.getBytes(UTF_8)
      head = 0
      val to = buf.length
      tail = to
      totalRead = 0
      mark = -1
      readString("")
    } finally this.buf = currBuf
  }

  def readString(default: String): String =
    if isNextToken('"', head) then {
      val pos = head
      val len = parseString(0, Math.min(tail - pos, charBuf.length), charBuf, pos)
      new String(charBuf, 0, len)
    } else readNullOrTokenError(default, '"')

  @tailrec
  private[this] def isNextToken(t: Byte, pos: Int): Boolean =
    if pos < tail then {
      val b = buf(pos)
      head = pos + 1
      b == t || ((b == ' ' || b == '\n' || (b | 0x4) == '\r') && nextToken(pos + 1) == t)
    } else isNextToken(t, loadMoreOrError(pos))

  @tailrec
  private[this] def nextToken(pos: Int): Byte =
    if pos < tail then {
      val b = buf(pos)
      if b == ' ' || b == '\n' || (b | 0x4) == '\r' then nextToken(pos + 1)
      else {
        head = pos + 1
        b
      }
    } else nextToken(loadMoreOrError(pos))

  @tailrec
  private[this] def readNullOrTokenError[@specialized A](default: A, t: Byte): A =
    if default != null then {
      val pos = head
      if pos != 0 then {
        if pos + 2 < tail then {
          val bs = ByteArrayAccess.getInt(buf, pos - 1)
          if bs == 0x6c6c756e then {
            head = pos + 3
            default
          } else tokenOrNullError(t, bs, pos)
        } else if buf(pos - 1) == 'n' then {
          head = loadMoreOrError(pos - 1) + 1
          readNullOrTokenError(default, t)
        } else tokenOrNullError(t)
      } else illegalTokenOperation()
    } else tokenError(t)

  @tailrec
  private[this] def parseString(i: Int, minLim: Int, charBuf: Array[Char], pos: Int): Int =
    if i + 3 < minLim then { // Based on SWAR routine of JSON string parsing: https://github.com/sirthias/borer/blob/fde9d1ce674d151b0fee1dd0c2565020c3f6633a/core/src/main/scala/io/bullet/borer/json/JsonParser.scala#L456
      val bs = ByteArrayAccess.getInt(buf, pos)
      val m = ((bs - 0x20202020 ^ 0x3c3c3c3c) - 0x1010101 | (bs ^ 0x5d5d5d5d) + 0x1010101) & 0x80808080
      charBuf(i) = (bs & 0xff).toChar
      charBuf(i + 1) = (bs >> 8 & 0xff).toChar
      charBuf(i + 2) = (bs >> 16 & 0xff).toChar
      charBuf(i + 3) = (bs >> 24).toChar
      if m != 0 then {
        val offset = java.lang.Integer.numberOfTrailingZeros(m) >> 3
        if (bs >> (offset << 3)).toByte == '"' then {
          head = pos + offset + 1
          i + offset
        } else parseEncodedString(i + offset, charBuf.length - 1, charBuf, pos + offset)
      } else parseString(i + 4, minLim, charBuf, pos + 4)
    } else if i < minLim then {
      val b = buf(pos)
      charBuf(i) = b.toChar
      if b == '"' then {
        head = pos + 1
        i
      } else if (b - 0x20 ^ 0x3c) <= 0 then parseEncodedString(i, charBuf.length - 1, charBuf, pos)
      else parseString(i + 1, minLim, charBuf, pos + 1)
    } else if pos >= tail then {
      val newPos = loadMoreOrError(pos)
      parseString(i, Math.min(charBuf.length, i + tail - newPos), charBuf, newPos)
    } else parseString(i, Math.min(growCharBuf(i + 1), i + tail - pos), this.charBuf, pos)

  @tailrec
  private[this] def parseEncodedString(i: Int, lim: Int, charBuf: Array[Char], pos: Int): Int = {
    val remaining = tail - pos
    if i < lim then {
      if remaining > 0 then {
        val b1 = buf(pos)
        if b1 >= 0 then {
          if b1 == '"' then {
            head = pos + 1
            i
          } else if b1 != '\\' then { // 0aaaaaaa (UTF-8 byte) -> 000000000aaaaaaa (UTF-16 char)
            if b1 < ' ' then unescapedControlCharacterError(pos)
            charBuf(i) = b1.toChar
            parseEncodedString(i + 1, lim, charBuf, pos + 1)
          } else if remaining > 1 then {
            val b2 = buf(pos + 1)
            if b2 != 'u' then {
              charBuf(i) = (b2: @switch) match {
                case '"'  => '"'
                case 'n'  => '\n'
                case 'r'  => '\r'
                case 't'  => '\t'
                case 'b'  => '\b'
                case 'f'  => '\f'
                case '\\' => '\\'
                case '/'  => '/'
                case _    => illegalEscapeSequenceError(pos + 1)
              }
              parseEncodedString(i + 1, lim, charBuf, pos + 2)
            } else if remaining > 5 then {
              val ch1 = readEscapedUnicode(pos + 2, buf)
              charBuf(i) = ch1
              if ch1 < 0xd800 || ch1 > 0xdfff then parseEncodedString(i + 1, lim, charBuf, pos + 6)
              else if remaining > 11 then {
                if buf(pos + 6) != '\\' then illegalEscapeSequenceError(pos + 6)
                if buf(pos + 7) != 'u' then illegalEscapeSequenceError(pos + 7)
                val ch2 = readEscapedUnicode(pos + 8, buf)
                if ch1 >= 0xdc00 || ch2 < 0xdc00 || ch2 > 0xdfff then decodeError("illegal surrogate character pair", pos + 11)
                charBuf(i + 1) = ch2
                parseEncodedString(i + 2, lim, charBuf, pos + 12)
              } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
          } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
        } else if (b1 >> 5) == -2 then { // 110bbbbb 10aaaaaa (UTF-8 bytes) -> 00000bbbbbaaaaaa (UTF-16 char)
          if remaining > 1 then {
            val b2 = buf(pos + 1)
            if (b1 & 0x1e) == 0 || (b2 & 0xc0) != 0x80 then malformedBytesError(b1, b2, pos)
            charBuf(i) = (b1 << 6 ^ b2 ^ 0xf80).toChar // 0xF80 == 0xC0.toByte << 6 ^ 0x80.toByte
            parseEncodedString(i + 1, lim, charBuf, pos + 2)
          } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
        } else if (b1 >> 4) == -2 then { // 1110cccc 10bbbbbb 10aaaaaa (UTF-8 bytes) -> ccccbbbbbbaaaaaa (UTF-16 char)
          if remaining > 2 then {
            val b2 = buf(pos + 1)
            val b3 = buf(pos + 2)
            val ch = (b1 << 12 ^ b2 << 6 ^ b3 ^ 0xfffe1f80).toChar // 0xFFFE1F80 == 0xE0.toByte << 12 ^ 0x80.toByte << 6 ^ 0x80.toByte
            if (b1 == -32 && (b2 & 0xe0) == 0x80) || (b2 & 0xc0) != 0x80 || (b3 & 0xc0) != 0x80 ||
              (ch >= 0xd800 && ch <= 0xdfff)
            then malformedBytesError(b1, b2, b3, pos)
            charBuf(i) = ch
            parseEncodedString(i + 1, lim, charBuf, pos + 3)
          } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
        } else if (b1 >> 3) == -2 then { // 11110ddd 10ddcccc 10bbbbbb 10aaaaaa (UTF-8 bytes) -> 110110uuuuccccbb 110111bbbbaaaaaa (UTF-16 chars), where uuuu = ddddd - 1
          if remaining > 3 then {
            val b2 = buf(pos + 1)
            val b3 = buf(pos + 2)
            val b4 = buf(pos + 3)
            val cp = b1 << 18 ^ b2 << 12 ^ b3 << 6 ^ b4 ^ 0x381f80 // 0x381F80 == 0xF0.toByte << 18 ^ 0x80.toByte << 12 ^ 0x80.toByte << 6 ^ 0x80.toByte
            if (b2 & 0xc0) != 0x80 || (b3 & 0xc0) != 0x80 || (b4 & 0xc0) != 0x80 ||
              cp < 0x10000 || cp > 0x10ffff
            then malformedBytesError(b1, b2, b3, b4, pos)
            charBuf(i) = ((cp >>> 10) + 0xd7c0).toChar // 0xD7C0 == 0xD800 - (0x10000 >>> 10)
            charBuf(i + 1) = ((cp & 0x3ff) + 0xdc00).toChar
            parseEncodedString(i + 2, lim, charBuf, pos + 4)
          } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
        } else malformedBytesError(b1, pos)
      } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
    } else parseEncodedString(i, growCharBuf(i + 2) - 1, this.charBuf, pos) // 2 is length of surrogate pair
  }

  private[this] def malformedBytesError(b1: Byte, pos: Int): Nothing = {
    var i = appendString("malformed byte(s): 0x", 0)
    i = appendHexByte(b1, i, hexDigits)
    decodeError(i, pos, null)
  }

  private[this] def malformedBytesError(b1: Byte, b2: Byte, pos: Int): Nothing = {
    val ds = hexDigits
    var i = appendString("malformed byte(s): 0x", 0)
    i = appendHexByte(b1, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b2, i, ds)
    decodeError(i, pos + 1, null)
  }

  private[this] def malformedBytesError(b1: Byte, b2: Byte, b3: Byte, pos: Int): Nothing = {
    val ds = hexDigits
    var i = appendString("malformed byte(s): 0x", 0)
    i = appendHexByte(b1, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b2, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b3, i, ds)
    decodeError(i, pos + 2, null)
  }

  private[this] def malformedBytesError(b1: Byte, b2: Byte, b3: Byte, b4: Byte, pos: Int): Nothing = {
    val ds = hexDigits
    var i = appendString("malformed byte(s): 0x", 0)
    i = appendHexByte(b1, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b2, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b3, i, ds)
    i = appendString(", 0x", i)
    i = appendHexByte(b4, i, ds)
    decodeError(i, pos + 3, null)
  }

  private[this] def appendHexByte(b: Byte, i: Int, ds: Array[Char]): Int = {
    ensureCharBufCapacity(i + 2)
    charBuf(i) = ds(b >> 4 & 0xf)
    charBuf(i + 1) = ds(b & 0xf)
    i + 2
  }

  private[this] def appendString(s: String, i: Int): Int = {
    val len = s.length
    val required = i + len
    ensureCharBufCapacity(required)
    s.getChars(0, len, charBuf, i)
    required
  }

  private[this] def ensureCharBufCapacity(required: Int): Unit =
    if charBuf.length < required then growCharBuf(required): Unit

  final private val hexDigits: Array[Char] =
    Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  private[this] def decodeError(msg: String, pos: Int, cause: Throwable = null): Nothing =
    decodeError(appendString(msg, 0), pos, cause)

  private[this] def decodeError(from: Int, pos: Int, cause: Throwable): Nothing = {
    var i = appendString(", offset: 0x", from)
    val offset =
      if (bbuf eq null) && (in eq null) then 0
      else totalRead - tail
    i = appendHexOffset(offset + pos, i)
    // if (config.appendHexDumpToParseException) {
    //   i = appendString(", buf:", i)
    //   i = appendHexDump(pos, offset.toInt, i)
    // }
    throw new JsonReaderException(new String(charBuf, 0, i), cause, true) // config.throwReaderExceptionWithStackTrace)
  }

  private[this] def appendHexOffset(d: Long, i: Int): Int = {
    ensureCharBufCapacity(i + 16)
    val ds = hexDigits
    var j = i
    val dl = d.toInt
    if dl != d then {
      val dh = (d >> 32).toInt
      var shift = 32 - java.lang.Integer.numberOfLeadingZeros(dh) & 0x1c
      while shift >= 0 do {
        charBuf(j) = ds(dh >> shift & 0xf)
        shift -= 4
        j += 1
      }
    }
    putHexInt(dl, j, charBuf, ds)
    j + 8
  }

  private[this] def putHexInt(d: Int, i: Int, charBuf: Array[Char], ds: Array[Char]): Unit = {
    charBuf(i) = ds(d >>> 28)
    charBuf(i + 1) = ds(d >> 24 & 0xf)
    charBuf(i + 2) = ds(d >> 20 & 0xf)
    charBuf(i + 3) = ds(d >> 16 & 0xf)
    charBuf(i + 4) = ds(d >> 12 & 0xf)
    charBuf(i + 5) = ds(d >> 8 & 0xf)
    charBuf(i + 6) = ds(d >> 4 & 0xf)
    charBuf(i + 7) = ds(d & 0xf)
  }

  private[this] def loadMoreOrError(pos: Int): Int = {
    if (bbuf eq null) && (in eq null) then endOfInputError()
    loadMore(pos, throwOnEndOfInput = true)
  }

  private[this] def loadMore(pos: Int): Int =
    if (bbuf eq null) && (in eq null) then pos
    else loadMore(pos, throwOnEndOfInput = false)

  private[this] def loadMore(pos: Int, throwOnEndOfInput: Boolean): Int = {
    var newPos = pos
    val offset =
      if mark < 0 then pos
      else mark
    if offset > 0 then {
      newPos -= offset
      val buf = this.buf
      val remaining = tail - offset
      var i = 0
      while i < remaining do {
        buf(i) = buf(i + offset)
        i += 1
      }
      if mark > 0 then mark = 0
      tail = remaining
      head = newPos
    } else growBuf()
    var len = buf.length - tail
    if bbuf ne null then {
      len = Math.min(bbuf.remaining, len)
      bbuf.get(buf, tail, len)
    } else len = Math.max(in.read(buf, tail, len), 0)
    if throwOnEndOfInput && len == 0 then endOfInputError()
    tail += len
    totalRead += len
    newPos
  }

  private[json] def endOfInputOrError(): Unit =
    if skipWhitespaces() then decodeError("expected end of input", head)

  private[this] def endOfInputError(): Nothing = decodeError("unexpected end of input", tail)
  private[this] def illegalEscapeSequenceError(pos: Int): Nothing = decodeError("illegal escape sequence", pos)
  private[this] def unescapedControlCharacterError(pos: Int): Nothing = decodeError("unescaped control character", pos)
  @tailrec
  private[this] def hexDigitError(pos: Int): Nothing = {
    if nibbles(buf(pos) & 0xff) < 0 then decodeError("expected hex digit", pos)
    hexDigitError(pos + 1)
  }
  private[this] def tokenError(t: Byte, pos: Int = head - 1): Nothing = {
    var i = appendString("expected '", 0)
    i = appendChar(t.toChar, i)
    i = appendChar('\'', i)
    decodeError(i, pos, null)
  }
  private[this] def tokenOrNullError(t: Byte, bs: Int, pos: Int): Nothing = tokenOrNullError(
    t, {
      val b0 = bs.toByte
      val b1 = (bs >> 8).toByte
      val b2 = (bs >> 16).toByte
      pos +
        (if b0 != 'n' then -1
         else if b1 != 'u' then 0
         else if b2 != 'l' then 1
         else 2)
    }
  )
  private[this] def tokenOrNullError(t: Byte, pos: Int = head - 1): Nothing = {
    var i = appendString("expected '", 0)
    i = appendChar(t.toChar, i)
    i = appendString("' or null", i)
    decodeError(i, pos, null)
  }
  private[this] def illegalTokenOperation(): Nothing =
    throw new IllegalStateException("expected preceding call of 'nextToken()' or 'isNextToken()'")

  private[this] def appendChar(ch: Char, i: Int): Int = {
    ensureCharBufCapacity(i + 1)
    charBuf(i) = ch
    i + 1
  }

  private[json] def skipWhitespaces(): Boolean = {
    var pos = head
    var buf = this.buf
    while (pos < tail || {
        pos = loadMore(pos)
        buf = this.buf
        pos < tail
      }) && {
        val b = buf(pos)
        b == ' ' || b == '\n' || (b | 0x4) == '\r'
      }
    do pos += 1
    head = pos
    pos != tail
  }

  private[this] def growBuf(): Unit = {
    var bufLen = buf.length
    // val maxBufSize = config.maxBufSize
    // if (bufLen == maxBufSize) tooLongInputError()
    bufLen <<= 1
    // if (bufLen > maxBufSize || bufLen < 0) bufLen = maxBufSize
    buf = java.util.Arrays.copyOf(buf, bufLen)
  }

  private[this] def growCharBuf(required: Int): Int = {
    var charBufLen = charBuf.length
    // val maxCharBufSize = config.maxCharBufSize
    // if (charBufLen == maxCharBufSize) tooLongStringError()
    charBufLen = (-1 >>> Integer.numberOfLeadingZeros(charBufLen | required)) + 1
    // if (charBufLen > maxCharBufSize || charBufLen < 0) charBufLen = maxCharBufSize
    charBuf = java.util.Arrays.copyOf(charBuf, charBufLen)
    charBufLen
  }

  private[this] def readEscapedUnicode(pos: Int, buf: Array[Byte]): Char = {
    val ns = nibbles
    val x =
      ns(buf(pos) & 0xff) << 12 |
        ns(buf(pos + 1) & 0xff) << 8 |
        ns(buf(pos + 2) & 0xff) << 4 |
        ns(buf(pos + 3) & 0xff)
    if x < 0 then hexDigitError(pos)
    x.toChar
  }

  final private val nibbles: Array[Byte] = Array(
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, -1,
    10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1
  )
