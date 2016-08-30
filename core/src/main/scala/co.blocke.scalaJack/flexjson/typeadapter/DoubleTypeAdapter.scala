package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, TokenType, Writer }

object DoubleTypeAdapter extends SimpleTypeAdapter[Double] {

  override def read(reader: Reader): Double = {
    reader.read(expected = TokenType.Number)
    reader.tokenText.toDouble
  }

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
