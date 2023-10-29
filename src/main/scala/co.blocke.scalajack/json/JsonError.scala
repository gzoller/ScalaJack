package co.blocke.scalajack
package json

class JsonError(msg: String) extends Throwable

abstract class ParseError(message: String) extends Throwable(message)
case class JsonParseError(message: String) extends ParseError(message)
case class CommaExpected(message: String = "") extends ParseError(message)
