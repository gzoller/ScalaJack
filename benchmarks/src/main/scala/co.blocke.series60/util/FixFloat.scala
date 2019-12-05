package co.blocke.series60
package util

object FixFloat {
  // Bizzare set of magic to try to "fix" the precision slop when moving from Float->Double (prints extra digits in JSON)
  def capFloat(f: Float): Double = {
    val d = f.toString.toDouble
    val diff = f.toDouble - d
    f - diff
  }
}
