package co.blocke.scalajack.shared

// A data structure encoding a simple algorithm for Trie pruning: Given a list
// of strings, and a sequence of incoming characters, find the strings that
// match, by manually maintaining a bitset. Empty strings are not allowed.
//
// ScalaJack: Removal of ZIO's original aliases feature for speed--we don't need this.
//
final class StringMatrix(val xs: Array[String]) {
//  println("StringMatrix: " + xs.toList.mkString(",") + "!")
  require(xs.forall(_.nonEmpty))
  require(xs.nonEmpty)
  require(xs.length < 64)

  val width = xs.length
  val height: Int = xs.map(_.length).max
  val lengths: Array[Int] = xs.map(_.length)
  val initial: Long = (0 until width).foldLeft(0L)((bs, r) => bs | (1L << r))

  private val initialAscii: Array[Long] = {
    val bits = new Array[Long](128)
    var string = 0
    while string < width do {
      val c = xs(string).charAt(0)
      if c < 128 then bits(c) |= 1L << string
      string += 1
    }
    bits
  }

  private val matrix: Array[Int] = {
    val m = Array.fill[Int](width * height)(-1)
    var string: Int = 0
    while string < width do {
      val s = xs(string)
      val len = s.length
      var char: Int = 0
      while char < len do {
        m(width * char + string) = s.codePointAt(char)
        char += 1
      }
      string += 1
    }
    m
  }

  private val resolve: Array[Int] = Array.tabulate[Int](xs.length)(identity)

  // must be called with increasing `char` (starting with bitset obtained from a
  // call to 'initial', char = 0)
  def update(bitset: Long, char: Int, c: Int): Long =
    if char >= height then 0L // too long
    else if bitset == 0L then 0L // everybody lost
    else if char == 0 && c < 128 then initialAscii(c)
    else {
      val base: Int = width * char

      if (bitset & (bitset - 1L)) == 0L then {
        val string = java.lang.Long.numberOfTrailingZeros(bitset)
        if matrix(base + string) == c then bitset else 0L
      } else if bitset == initial then { // special case when it is dense since it is simple
        var latest: Long = bitset
        var string: Int = 0
        while string < width do {
          if matrix(base + string) != c then latest = latest ^ (1L << string)
          string += 1
        }
        latest
      } else {
        var latest: Long = bitset
        var remaining: Long = bitset
        while remaining != 0L do {
          val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
          val bit: Long = 1L << string
          if matrix(base + string) != c then latest = latest ^ bit
          remaining = remaining ^ bit
        }
        latest
      }
    }

  // excludes entries that are not the given exact length
  def exact(bitset: Long, length: Int): Long =
    if length > height then 0L // too long
    else if bitset == 0L then 0L
    else if (bitset & (bitset - 1L)) == 0L then if lengths(java.lang.Long.numberOfTrailingZeros(bitset)) == length then bitset else 0L
    else {
      var latest: Long = bitset
      var remaining: Long = bitset
      while remaining != 0L do {
        val string: Int = java.lang.Long.numberOfTrailingZeros(remaining)
        val bit: Long = 1L << string
        if lengths(string) != length then latest = latest ^ bit
        remaining = remaining ^ bit
      }
      latest
    }

  def first(bitset: Long): Int =
    if bitset == 0L then -1
    else resolve(java.lang.Long.numberOfTrailingZeros(bitset)) // never returns 64
}
