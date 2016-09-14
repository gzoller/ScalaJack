package co.blocke.scalajack.json
package typeadapter
package javaprimitives

object JavaLongTypeAdapter extends SimpleTypeAdapter[java.lang.Long] {

  override def read(reader: Reader): java.lang.Long =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        java.lang.Long.valueOf(reader.readLong())
    }

  override def write(value: java.lang.Long, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeLong(value.longValue)
    }

}
