package co.blocke.scalajack.flexjson

object TokenType extends Enumeration {
  type TokenType = Value

  val Nothing,
      BeginObject,
      EndObject,
      BeginArray,
      EndArray,
      Identifier,
      LiteralName, // null, true, false
      Number,
      String,
      True,
      False,
      Null,
      NameSeparator,
      ValueSeparator,
      InsignificantWhitespace,
      End = Value
}
