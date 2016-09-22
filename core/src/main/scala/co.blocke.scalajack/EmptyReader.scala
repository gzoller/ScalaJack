package co.blocke.scalajack

import TokenType.TokenType

object EmptyReader extends Reader {

  override def peek: TokenType = TokenType.Nothing

  override def read(expected: TokenType): Unit = ???

  override def readString(): String = ???

  override def tokenText: String = ???

  override var position: Int = _

  override def read(): TokenType = ???

  override def source: Array[Char] = ???

  override def tokenOffset: Int = ???

  override def tokenLength: Int = ???

  override def tokenOffsetAt(position: Int): Int = ???

  override def tokenLengthAt(position: Int): Int = ???

}
