package co.blocke.scalajack
package json
package reading

import java.nio.CharBuffer
import java.util.Arrays

// like StringBuilder but doesn't have any encoding or range checks
final class FastStringBuilder(initial: Int = 16) {
  private[this] var chars: Array[Char] = new Array[Char](initial)
  private[this] var i: Int = 0

  def append(c: Char): FastStringBuilder = {
    if i == chars.length then chars = Arrays.copyOf(chars, chars.length * 2)
    chars(i) = c
    i += 1
    this
  }

  def buffer: CharSequence = CharBuffer.wrap(chars, 0, i)
}
