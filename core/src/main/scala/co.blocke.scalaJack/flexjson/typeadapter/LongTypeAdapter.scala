package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

object LongTypeAdapter extends SimpleTypeAdapter[Long] {

  override def read(reader: Reader): Long =
    reader.readLong()

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
