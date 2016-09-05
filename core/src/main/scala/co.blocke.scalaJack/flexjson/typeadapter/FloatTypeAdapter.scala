package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

object FloatTypeAdapter extends SimpleTypeAdapter[Float] {

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
