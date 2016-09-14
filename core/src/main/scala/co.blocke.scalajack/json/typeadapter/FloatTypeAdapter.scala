package co.blocke.scalajack.json
package typeadapter

object FloatTypeAdapter extends SimpleTypeAdapter[Float] {

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
