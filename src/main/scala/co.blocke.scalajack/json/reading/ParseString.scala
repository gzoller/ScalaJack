package co.blocke.scalajack
package json
package reading

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import java.io.InputStream
import co.blocke.scalajack.util.ByteArrayAccess

case class ParseString(var buf: Array[Byte], var charBuf: Array[Char] = new Array[Char](4096)):

    // var buf: Array[Byte] = new Array[Byte](32768)
    var head: Int = 0
    var tail: Int = buf.length
    // var mark: Int = -1
    // var bbuf: ByteBuffer = null
    // var in: InputStream = null
    val maxCharBufSize = 4194304

    /**
      * 
      *
      * @param i - position in charBuf to start populating
      * @param minLim -- ???
      * @param charBuf -- Array containing characters of the resultant parsed string
      * @param pos -- position in the input buffer to start reading from (ie current parse position)
      * @return number of characters read
      */
    @tailrec
    final def parseString(i: Int, minLim: Int, charBuf: Array[Char], pos: Int): Int =
        if (i + 3 < minLim) { // Based on SWAR routine of JSON string parsing: https://github.com/sirthias/borer/blob/fde9d1ce674d151b0fee1dd0c2565020c3f6633a/core/src/main/scala/io/bullet/borer/json/JsonParser.scala#L456
            val bs = ByteArrayAccess.getInt(buf, pos)
            val m = ((bs - 0x20202020 ^ 0x3C3C3C3C) - 0x1010101 | (bs ^ 0x5D5D5D5D) + 0x1010101) & 0x80808080
            charBuf(i) = (bs & 0xFF).toChar
            charBuf(i + 1) = (bs >> 8 & 0xFF).toChar
            charBuf(i + 2) = (bs >> 16 & 0xFF).toChar
            charBuf(i + 3) = (bs >> 24).toChar
            if (m != 0) {
                val offset = java.lang.Integer.numberOfTrailingZeros(m) >> 3
                if ((bs >> (offset << 3)).toByte == '"') {
                head = pos + offset + 1
                i + offset
                } else throw new Exception("special chars found 1") //else parseEncodedString(i + offset, charBuf.length - 1, charBuf, pos + offset)
            } else parseString(i + 4, minLim, charBuf, pos + 4)
        } else if (i < minLim) {
            val b = buf(pos)
            charBuf(i) = b.toChar
            if (b == '"') {
                head = pos + 1
                i
            } else if ((b - 0x20 ^ 0x3C) <= 0) throw new Exception("special chars found 2") //parseEncodedString(i, charBuf.length - 1, charBuf, pos)
            else parseString(i + 1, minLim, charBuf, pos + 1)
        } else if (pos >= tail) {
            throw new Exception("Buffer overlow while parsing string")
        } else 
            parseString(i, Math.min(growCharBuf(i + 1), i + tail - pos), this.charBuf, pos)

        /*
    @tailrec
    private[this] def parseEncodedString(i: Int, lim: Int, charBuf: Array[Char], pos: Int): Int = {
        val remaining = tail - pos
        if (i < lim) {
        if (remaining > 0) {
            val b1 = buf(pos)
            if (b1 >= 0) {
            if (b1 == '"') {
                head = pos + 1
                i
            } else if (b1 != '\\') { // 0aaaaaaa (UTF-8 byte) -> 000000000aaaaaaa (UTF-16 char)
                if (b1 < ' ') throw new Exception(s"unescapedControlCharacterError($pos)")
                charBuf(i) = b1.toChar
                parseEncodedString(i + 1, lim, charBuf, pos + 1)
            } else if (remaining > 1) {
                val b2 = buf(pos + 1)
                if (b2 != 'u') {
                charBuf(i) = (b2: @switch) match {
                    case '"' => '"'
                    case 'n' => '\n'
                    case 'r' => '\r'
                    case 't' => '\t'
                    case 'b' => '\b'
                    case 'f' => '\f'
                    case '\\' => '\\'
                    case '/' => '/'
                    case _ => throw new Exception(s"escapeSequenceError(${pos + 1})")
                }
                parseEncodedString(i + 1, lim, charBuf, pos + 2)
                } else if (remaining > 5) {
                val ch1 = readEscapedUnicode(pos + 2, buf)
                charBuf(i) = ch1
                if ((ch1 & 0xF800) != 0xD800) parseEncodedString(i + 1, lim, charBuf, pos + 6)
                else if (remaining > 11) {
                    if (buf(pos + 6) != '\\') throw new Exception(s"escapeSequenceError(${pos + 6})")
                    if (buf(pos + 7) != 'u') throw new Exception(s"escapeSequenceError(${pos + 7})")
                    val ch2 = readEscapedUnicode(pos + 8, buf)
                    charBuf(i + 1) = ch2
                    if (ch1 >= 0xDC00 || (ch2 & 0xFC00) != 0xDC00) throw new Exception(s"decodeError(\"illegal surrogate character pair\", ${pos + 11})")
                    parseEncodedString(i + 2, lim, charBuf, pos + 12)
                } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
                } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else if ((b1 & 0xE0) == 0xC0) { // 110bbbbb 10aaaaaa (UTF-8 bytes) -> 00000bbbbbaaaaaa (UTF-16 char)
            if (remaining > 1) {
                val b2 = buf(pos + 1)
                val ch = (b1 << 6 ^ b2 ^ 0xF80).toChar // 0xF80 == 0xC0.toByte << 6 ^ 0x80.toByte
                charBuf(i) = ch
                if ((b2 & 0xC0) != 0x80 || ch < 0x80) throw new Exception(s"malformedBytesError($b1, $b2, $pos)")
                parseEncodedString(i + 1, lim, charBuf, pos + 2)
            } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else if ((b1 & 0xF0) == 0xE0) { // 1110cccc 10bbbbbb 10aaaaaa (UTF-8 bytes) -> ccccbbbbbbaaaaaa (UTF-16 char)
            if (remaining > 2) {
                val b2 = buf(pos + 1)
                val b3 = buf(pos + 2)
                val ch = (b1 << 12 ^ b2 << 6 ^ b3 ^ 0x1F80).toChar // 0x1F80 == (0x80.toByte << 6 ^ 0x80.toByte).toChar
                charBuf(i) = ch
                if ((b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 || ch < 0x800 ||
                (ch & 0xF800) == 0xD800) throw new Exception(s"malformedBytesError($b1, $b2, $b3, $pos)")
                parseEncodedString(i + 1, lim, charBuf, pos + 3)
            } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else if ((b1 & 0xF8) == 0xF0) { // 11110ddd 10ddcccc 10bbbbbb 10aaaaaa (UTF-8 bytes) -> 110110uuuuccccbb 110111bbbbaaaaaa (UTF-16 chars), where uuuu = ddddd - 1
            if (remaining > 3) {
                val b2 = buf(pos + 1)
                val b3 = buf(pos + 2)
                val b4 = buf(pos + 3)
                val cp = b1 << 18 ^ b2 << 12 ^ b3 << 6 ^ b4 ^ 0x381F80 // 0x381F80 == 0xF0.toByte << 18 ^ 0x80.toByte << 12 ^ 0x80.toByte << 6 ^ 0x80.toByte
                val ch1 = ((cp >>> 10) + 0xD7C0).toChar // 0xD7C0 == 0xD800 - (0x10000 >>> 10)
                charBuf(i) = ch1
                charBuf(i + 1) = ((cp & 0x3FF) + 0xDC00).toChar
                if ((b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 ||
                (ch1 & 0xF800) != 0xD800) throw new Exception(s"malformedBytesError($b1, $b2, $b3, $b4, $pos)")
                parseEncodedString(i + 2, lim, charBuf, pos + 4)
            } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
            } else throw new Exception(s"malformedBytesError($b1, $pos)")
        } else parseEncodedString(i, lim, charBuf, loadMoreOrError(pos))
        } else parseEncodedString(i, growCharBuf(i + 2) - 1, this.charBuf, pos) // 2 is length of surrogate pair
    }

    private[this] def readEscapedUnicode(pos: Int, buf: Array[Byte]): Char = {
        val ns = nibbles
        val x =
        ns(buf(pos) & 0xFF) << 12 |
            ns(buf(pos + 1) & 0xFF) << 8 |
            ns(buf(pos + 2) & 0xFF) << 4 |
            ns(buf(pos + 3) & 0xFF)
        if (x < 0) throw new Exception(s"hexDigitError($pos)")
        x.toChar
    }
    */

    private[this] def growCharBuf(required: Int): Int = {
        var charBufLen = charBuf.length
        if (charBufLen == maxCharBufSize) throw new Exception("tooLongStringError")
        charBufLen = (-1 >>> Integer.numberOfLeadingZeros(charBufLen | required)) + 1
        if (charBufLen > maxCharBufSize || charBufLen < 0) charBufLen = maxCharBufSize
        charBuf = java.util.Arrays.copyOf(charBuf, charBufLen)
        charBufLen
    }

    /*
    private final val nibbles: Array[Byte] = Array(
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
    )
    */

    /*
    private[this] def loadMoreOrError(pos: Int): Int = {
        if ((bbuf eq null) && (in eq null)) throw new Exception("endOfInputError")
        loadMore(pos, throwOnEndOfInput = true)
    }

    private[this] def loadMore(pos: Int): Int =
        if ((bbuf eq null) && (in eq null)) pos
        else loadMore(pos, throwOnEndOfInput = false)

    private[this] def loadMore(pos: Int, throwOnEndOfInput: Boolean): Int = {
        var newPos = pos
        val offset =
        if (mark < 0) pos
        else mark
        if (offset > 0) {
        newPos -= offset
        val buf = this.buf
        val remaining = tail - offset
        var i = 0
        while (i < remaining) {
            buf(i) = buf(i + offset)
            i += 1
        }
        if (mark > 0) mark = 0
        tail = remaining
        head = newPos
        } else growBuf()
        var len = buf.length - tail
        if (bbuf ne null) {
        len = Math.min(bbuf.remaining, len)
        bbuf.get(buf, tail, len)
        } else len = Math.max(in.read(buf, tail, len), 0)
        if (throwOnEndOfInput && len == 0) throw new Exception("endOfInputError")
        tail += len
        totalRead += len
        newPos
    }

    private[this] def growBuf(): Unit = {
        var bufLen = buf.length
        if (bufLen == maxBufSize) throw new Exception("tooLongInputError")
        bufLen <<= 1
        if (bufLen > maxBufSize || bufLen < 0) bufLen = maxBufSize
        buf = java.util.Arrays.copyOf(buf, bufLen)
    }
        */
