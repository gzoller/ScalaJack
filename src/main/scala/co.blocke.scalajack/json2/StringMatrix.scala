package co.blocke.scalajack
package json2

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
//
// 
final class StringMatrix(val xs: Array[String], aliases: Array[(String, Int)] = Array.empty) {
  require(xs.forall(_.nonEmpty))
  require(xs.nonEmpty)
  require(xs.length + aliases.length < 64)
  require(aliases.forall(_._1.nonEmpty))
  require(aliases.forall(p => p._2 >= 0 && p._2 < xs.length))

  val width               = xs.length + aliases.length
  val height: Int         = xs.map(_.length).max max (if (aliases.isEmpty) 0 else aliases.map(_._1.length).max)
  val lengths: Array[Int] = xs.map(_.length) ++ aliases.map(_._1.length)
  val initial: Long       = (0 until width).foldLeft(0L)((bs, r) => bs | (1L << r))

  private val matrix: Array[Int] = {
    val m           = Array.fill[Int](width * height)(-1)
    var string: Int = 0
    while (string < width) {
      val s         = if (string < xs.length) xs(string) else aliases(string - xs.length)._1
      val len       = s.length
      var char: Int = 0
      while (char < len) {
        m(width * char + string) = s.codePointAt(char)
        char += 1
      }
      string += 1
    }
    m
  }

  private val resolve: Array[Int] = {
    val r = Array.tabulate[Int](xs.length + aliases.length)(identity)
    aliases.zipWithIndex.foreach { case ((_, pi), i) => r(xs.length + i) = pi }
    r
  }

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Int): Long =
    if (char >= height) 0L    // too long
    else if (bitset == 0L) 0L // everybody lost
    else {
      var latest: Long = bitset
      val base: Int    = width * char

      if (bitset == initial) { // special case when it is dense since it is simple
        var string: Int = 0
        while (string < width) {
          if (matrix(base + string) != c)
            latest = latest ^ (1L << string)
          string += 1
        }
      } else {
        var remaining: Long = bitset
        while (remaining != 0L) {
          val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit: Long   = 1L << string
          if (matrix(base + string) != c)
            latest = latest ^ bit
          remaining = remaining ^ bit
        }
      }

      latest
    }

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if (length > height) 0L // too long
    else {
      var latest: Long    = bitset
      var remaining: Long = bitset
      while (remaining != 0L) {
        val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit: Long   = 1L << string
        if (lengths(string) != length)
          latest = latest ^ bit
        remaining = remaining ^ bit
      }
      latest
    }

  def first(bitset: Long): Int =
    if (bitset == 0L) -1
    else resolve(java.lang.Long.numberOfTrailingZeros(bitset)) // never returns 64
}