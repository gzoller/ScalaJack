package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object JavaCharacterTypeAdapter extends SimpleTypeAdapter[java.lang.Character] {

  override def read(reader: Reader): java.lang.Character =
    reader.peek match {
      case TokenType.Null ⇒
        null

      case TokenType.String ⇒
        reader.read(expected = TokenType.String)
        java.lang.Character.valueOf(reader.tokenText.head)
    }

  override def write(value: java.lang.Character, writer: Writer): Unit = ???

}
