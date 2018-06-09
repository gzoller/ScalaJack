package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends TypeAdapter.=:=[Long] {

  override def read(reader: Reader): Long =
    reader.readLong()

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
