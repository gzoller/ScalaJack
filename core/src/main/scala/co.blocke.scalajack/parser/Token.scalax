package co.blocke.scalajack.parser

sealed trait Token {
  val label: String
}
sealed trait SimpleToken extends Token

case class DecimalToken( d: Double )  extends SimpleToken{ val label: String = "Decimal" }
case class IntToken( i: Int )         extends SimpleToken{ val label: String = "Int" }
case class BooleanToken( b: Boolean ) extends SimpleToken{ val label: String = "Boolean" }
case object NullToken                 extends SimpleToken{ val label: String = "Null" }
case class StringToken( s: String )   extends SimpleToken{ val label: String = "String" }

case object ArrayToken extends Token{ val label: String = "Array" }
case object EndArrayToken extends Token{ val label: String = "End Array" }
case object ObjectToken extends Token{ val label: String = "Object" }
case object EndObjectToken extends Token{ val label: String = "End Object" }

case class KVPairToken(key: Token, value: Token) extends Token{ val label: String = "Key Value Pair" }
case class ErrorToken(msg: String) extends Token{ val label: String = "Error" }

// Optional Tokens, e.g. not JSON compatible but maybe for other protocols
case object MapToken extends Token{ val label: String = "Map" }
case object EndMapToken extends Token{ val label: String = "End Map" }
case class CustomToken(label: String) extends Token