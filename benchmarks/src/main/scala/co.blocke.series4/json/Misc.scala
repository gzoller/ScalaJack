package co.blocke.series4
package json

case class JsonParseException(msg: String, pos: Int, incompleteObject: Boolean = false) extends Exception(msg) {
  def toString(s: StringBuilder) = s + "\n" + s"%${pos + 1}s" format "^"
}

trait JsonTokenizer {
  def tokenize(s: Array[Char]): JsonIndex
}