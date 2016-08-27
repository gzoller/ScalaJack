package co.blocke.scalajack.flexjson
import co.blocke.scalajack.flexjson.TokenType.TokenType

object EmptyReader extends Reader {

  override def peek: TokenType = TokenType.Nothing

  override def read(expected: TokenType): Unit = ???

  override def readString(): String = ???

  override def tokenText: String = ???

}
