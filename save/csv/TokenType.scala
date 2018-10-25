package co.blocke.scalajack

object TokenType extends Enumeration {
  type TokenType = Value

  val BeginObject, EndObject, BeginArray, EndArray, ValueSeparator, KeySeparator, ObjectKey, Number, String, True, False, Null, UnknownLiteralName, End = Value
}
