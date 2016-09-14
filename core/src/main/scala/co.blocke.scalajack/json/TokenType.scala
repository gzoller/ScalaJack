package co.blocke.scalajack.json

object TokenType extends Enumeration {
  type TokenType = Value

  val Nothing, BeginObject, EndObject, BeginArray, EndArray, Number, String, True, False, Null, UnknownLiteralName, End = Value
}
