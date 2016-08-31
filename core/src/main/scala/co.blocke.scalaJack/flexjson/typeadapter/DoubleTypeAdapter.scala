package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

object DoubleTypeAdapter extends SimpleTypeAdapter[Double] {

  override def read(reader: Reader): Double =
    reader.readDouble()

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
