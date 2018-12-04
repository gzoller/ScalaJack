package co.blocke.scalajack
package json

import parser._

case class JsonTokenGenerator( json: String ) extends TokenGenerator {

  override def nextToken(isKeyValuePair: Boolean): Token =
    if( pos >= max )
      ErrorToken("read past end of json input")
    else {
      skipWhitespace()
      js(pos) match {
        case '{' =>
        case '[' =>
        case '"' =>
        case 't' =>
        case 'n' =>
      }
    }
      NullToken()

  private var pos = 0
  private var js = json.toCharArray
  private var max = js.length

  private def skipWhitespace(): Unit = while( js(pos).isWhitespace && pos < max ) pos += 1
}