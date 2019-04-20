package co.blocke.scalajack
package json4s

import model._
import model.TokenType._
import org.json4s._

case class JValueTokenizer() extends Tokenizer[JValue] {

  def tokenize(source: JValue): java.util.ArrayList[ParseToken[JValue]] = {
    val tokenspace = new java.util.ArrayList[ParseToken[JValue]]()

    def consumeValue(value: JValue): Unit =
      value match {
        case JNull =>
          tokenspace.add(JValueToken(JNull, TokenType.Null))
        case v: JArray =>
          tokenspace.add(JValueToken(JString("array"), TokenType.BeginArray))
          v.children.foreach(consumeValue(_))
          tokenspace.add(JValueToken(JString("array"), TokenType.EndArray))

        case v: JBool =>
          tokenspace.add(JValueToken(v, TokenType.Boolean))
        case v: JDecimal =>
          tokenspace.add(JValueToken(v, TokenType.Number))
        case v: JDouble =>
          tokenspace.add(JValueToken(v, TokenType.Number))
        case v: JInt =>
          tokenspace.add(JValueToken(v, TokenType.Number))
        case v: JLong =>
          tokenspace.add(JValueToken(v, TokenType.Number))

        case v: JObject =>
          tokenspace.add(JValueToken(new JString("object"), TokenType.BeginObject))
          v.obj.foreach {
            case (objKey, objValue) =>
              tokenspace.add(JValueToken(JString(objKey), String))
              consumeValue(objValue)
          }
          tokenspace.add(JValueToken(new JString("object"), TokenType.EndObject))

        case v: JString =>
          tokenspace.add(JValueToken(v, TokenType.String))
      }

    consumeValue(source)

    tokenspace.add(JValueToken(null, End))
    tokenspace
  }
}
