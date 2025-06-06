package co.blocke.scalajack.internal

import co.blocke.scalajack.IllegalCharacterError

import java.nio.CharBuffer
import java.util.Arrays
import scala.annotation.tailrec

// like StringBuilder but doesn't have any encoding or range checks
final class FastStringBuilder(initial: Int = 16) {
  private var chars: Array[Char] = new Array[Char](initial)
  private var i: Int = 0

  def clear() = i = 0
  def length = i
  def setLength(ni: Int) = i = ni

  @tailrec
  final def ensureCapacity(size: Int): Unit =
    if i + size < chars.length then ()
    else
      chars = Arrays.copyOf(chars, chars.length * 2)
      ensureCapacity(size)

  def append(c: Char): Unit =
    if i == chars.length then chars = Arrays.copyOf(chars, chars.length * 2)
    chars(i) = c
    i += 1

  def peekBack(): Option[Char] =
    if i > 0 then Some(chars(i - 1)) else None

  def backspace(): Unit =
    if i > 0 then i -= 1

  final private val escapedChars: Array[Byte] = Array(
    -1, -1, -1, -1, -1, -1, -1, -1, 98, 116, 110, -1, 102, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1
  )

  private def appendEscapedUnicode(c: Char): Unit = {
    append('\\')
    append('u')
    append("%04x".format(c.toInt))
  }

  @tailrec
  final def appendEscaped(s: String, from: Int, to: Int): Unit =
    ensureCapacity(2)
    if from >= to then ()
    else
      val ch1 = s.charAt(from)
      if ch1 < 0x80 then
        val esc = escapedChars(ch1)
        if esc == 0 then
          chars(i) = ch1
          i += 1
          appendEscaped(s, from + 1, to)
        else if esc > 0 then
          chars(i) = 0x5c // double quote
          chars(i + 1) = esc.toChar
          i += 2
          appendEscaped(s, from + 1, to)
        else appendEscapedUnicode(ch1)
      else if (ch1 & 0xf800) != 0xd800 then appendEscapedUnicode(ch1)
      else
        var ch2 = 0
        if ch1 >= 0xdc00 || from + 1 >= to || {
            ch2 = s.charAt(from + 1).toInt
            (ch2 & 0xfc00) != 0xdc00
          }
        then throw new IllegalCharacterError("Illegal encoded text character in string value: " + ch2)
        appendEscapedUnicode(ch2.toChar)

  def append(s: String): Unit =
    ensureCapacity(s.length)
    s.getChars(0, s.length, chars, i)
    i += s.length

  def append(v: scala.math.BigDecimal): Unit = append(v.toString)
  def append(v: scala.math.BigInt): Unit = append(v.toString)
  def append(v: Boolean): Unit = append(v.toString)
  def append(v: Double): Unit = append(v.toString)
  def append(v: Float): Unit = append(v.toString)
  def append(v: Int): Unit = append(v.toString)
  def append(v: Long): Unit = append(v.toString)
  def append(v: Short): Unit = append(v.toString)
  def append(v: java.lang.Number): Unit = append(v.toString)

  def result = CharBuffer.wrap(chars, 0, i).toString
}
