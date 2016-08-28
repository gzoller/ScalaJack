package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, TokenType, Writer}

object FloatTypeAdapter extends SimpleTypeAdapter[Float] {

  override def read(reader: Reader): Float = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toFloat
  }

  override def write(value: Float, writer: Writer): Unit = ???

}
