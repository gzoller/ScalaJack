package co.blocke.scalajack
package json

class JsonError(msg: String) extends Throwable

case class JsonParseError(message: String) extends parser.ParseError(message)
case class CommaExpected(message: String = "") extends parser.ParseError(message)
