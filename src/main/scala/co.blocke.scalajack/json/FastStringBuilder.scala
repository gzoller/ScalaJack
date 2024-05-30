package co.blocke.scalajack
package json

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

  final private val escapedChars: Array[Byte] = Array(
    -1, -1, -1, -1, -1, -1, -1, -1, 98, 116, 110, -1, 102, 114, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1
  )
  final private val lowerCaseHexDigits: Array[Short] = Array(
    12336, 12592, 12848, 13104, 13360, 13616, 13872, 14128, 14384, 14640, 24880, 25136, 25392, 25648, 25904, 26160, 12337, 12593, 12849, 13105, 13361, 13617, 13873, 14129, 14385, 14641, 24881, 25137, 25393, 25649, 25905, 26161, 12338, 12594, 12850, 13106,
    13362, 13618, 13874, 14130, 14386, 14642, 24882, 25138, 25394, 25650, 25906, 26162, 12339, 12595, 12851, 13107, 13363, 13619, 13875, 14131, 14387, 14643, 24883, 25139, 25395, 25651, 25907, 26163, 12340, 12596, 12852, 13108, 13364, 13620, 13876, 14132,
    14388, 14644, 24884, 25140, 25396, 25652, 25908, 26164, 12341, 12597, 12853, 13109, 13365, 13621, 13877, 14133, 14389, 14645, 24885, 25141, 25397, 25653, 25909, 26165, 12342, 12598, 12854, 13110, 13366, 13622, 13878, 14134, 14390, 14646, 24886, 25142,
    25398, 25654, 25910, 26166, 12343, 12599, 12855, 13111, 13367, 13623, 13879, 14135, 14391, 14647, 24887, 25143, 25399, 25655, 25911, 26167, 12344, 12600, 12856, 13112, 13368, 13624, 13880, 14136, 14392, 14648, 24888, 25144, 25400, 25656, 25912, 26168,
    12345, 12601, 12857, 13113, 13369, 13625, 13881, 14137, 14393, 14649, 24889, 25145, 25401, 25657, 25913, 26169, 12385, 12641, 12897, 13153, 13409, 13665, 13921, 14177, 14433, 14689, 24929, 25185, 25441, 25697, 25953, 26209, 12386, 12642, 12898, 13154,
    13410, 13666, 13922, 14178, 14434, 14690, 24930, 25186, 25442, 25698, 25954, 26210, 12387, 12643, 12899, 13155, 13411, 13667, 13923, 14179, 14435, 14691, 24931, 25187, 25443, 25699, 25955, 26211, 12388, 12644, 12900, 13156, 13412, 13668, 13924, 14180,
    14436, 14692, 24932, 25188, 25444, 25700, 25956, 26212, 12389, 12645, 12901, 13157, 13413, 13669, 13925, 14181, 14437, 14693, 24933, 25189, 25445, 25701, 25957, 26213, 12390, 12646, 12902, 13158, 13414, 13670, 13926, 14182, 14438, 14694, 24934, 25190,
    25446, 25702, 25958, 26214
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
        then throw new JsonIllegalCharacterError("Illegal encoded text character in string value: " + ch2)
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
