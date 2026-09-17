package co.blocke.scalajack.shared

import co.blocke.scalajack.IllegalCharacterError

import java.nio.CharBuffer
import java.util.Arrays

// like StringBuilder but doesn't have any encoding or range checks
final class FastStringBuilder(initial: Int = 16) {
  private var chars: Array[Char] = new Array[Char](initial)
  private var i: Int = 0

  def clear() = i = 0
  def length = i
  def setLength(ni: Int) = i = ni

  final def ensureCapacity(size: Int): Unit =
    val required = i + size
    if required > chars.length then chars = Arrays.copyOf(chars, Math.max(required, Math.max(1, chars.length << 1)))

  def append(c: Char): Unit =
    if i == chars.length then chars = Arrays.copyOf(chars, Math.max(1, chars.length << 1))
    chars(i) = c
    i += 1

  def peekBack(): Option[Char] =
    if i > 0 then Some(chars(i - 1)) else None

  def backspace(): Unit =
    if i > 0 then i -= 1

  private inline def appendEscapedUnicode(c: Char): Unit =
    ensureCapacity(6)
    val hex = FastStringBuilder.hexDigits
    val n = c.toInt
    chars(i) = '\\'
    chars(i + 1) = 'u'
    chars(i + 2) = hex(n >>> 12)
    chars(i + 3) = hex(n >>> 8 & 0xf)
    chars(i + 4) = hex(n >>> 4 & 0xf)
    chars(i + 5) = hex(n & 0xf)
    i += 6

  final def appendEscaped(s: String, from: Int, to: Int): Unit =
    var p = from
    while p < to do
      ensureCapacity(2)
      val ch1 = s.charAt(p)
      if ch1 < 0x80 then
        val esc = FastStringBuilder.escapedChars(ch1)
        if esc == 0 then
          chars(i) = ch1
          i += 1
        else if esc > 0 then
          chars(i) = 0x5c
          chars(i + 1) = esc.toChar
          i += 2
        else appendEscapedUnicode(ch1)
      else if (ch1 & 0xf800) != 0xd800 then appendEscapedUnicode(ch1)
      else
        var ch2 = 0.toChar
        if ch1 >= 0xdc00 || p + 1 >= to || {
            ch2 = s.charAt(p + 1)
            (ch2 & 0xfc00) != 0xdc00
          }
        then throw new IllegalCharacterError("Illegal encoded text character in string value: " + ch2)
        appendEscapedUnicode(ch1)
        appendEscapedUnicode(ch2)
        p += 1
      p += 1

  def append(s: String): Unit =
    ensureCapacity(s.length)
    s.getChars(0, s.length, chars, i)
    i += s.length

  def append(v: scala.math.BigDecimal): Unit = append(v.toString)
  def append(v: scala.math.BigInt): Unit = append(v.toString)
  def append(v: Boolean): Unit = append(if v then "true" else "false")
  def append(v: Double): Unit = append(v.toString)
  def append(v: Float): Unit = append(v.toString)
  def append(v: Int): Unit = append(v.toLong)

  def append(v: Long): Unit =
    if v == Long.MinValue then append("-9223372036854775808")
    else
      ensureCapacity(20)
      var x = if v < 0 then -v else v
      var digits = 1
      var limit = 10L
      while digits < 19 && x >= limit do
        digits += 1
        limit *= 10
      if v < 0 then
        chars(i) = '-'
        i += 1
      var p = i + digits
      i = p
      val digitPairs = FastStringBuilder.digitPairs
      while x >= 100 do
        val q = x / 100
        val r = (x - q * 100).toInt << 1
        p -= 2
        chars(p) = digitPairs(r)
        chars(p + 1) = digitPairs(r + 1)
        x = q
      if x < 10 then chars(p - 1) = ('0' + x.toInt).toChar
      else
        val r = x.toInt << 1
        chars(p - 2) = digitPairs(r)
        chars(p - 1) = digitPairs(r + 1)

  def append(v: Short): Unit = append(v.toInt)
  def append(v: java.lang.Number): Unit = append(v.toString)

  def result = CharBuffer.wrap(chars, 0, i).toString
}

object FastStringBuilder:
  private[shared] val escapedChars: Array[Byte] = Array(
    -1, -1, -1, -1, -1, -1, -1, -1, 98, 116, 110, -1, 102, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1
  )
  private[shared] val hexDigits: Array[Char] = "0123456789abcdef".toCharArray
  private[shared] val digitPairs: Array[Char] =
    val result = new Array[Char](200)
    var n = 0
    while n < 100 do
      result(n << 1) = ('0' + n / 10).toChar
      result((n << 1) + 1) = ('0' + n % 10).toChar
      n += 1
    result
