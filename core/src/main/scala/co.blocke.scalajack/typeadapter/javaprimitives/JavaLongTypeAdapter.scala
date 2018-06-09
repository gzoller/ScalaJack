package co.blocke.scalajack
package typeadapter
package javaprimitives

object JavaLongTypeAdapter extends TypeAdapter.=:=[java.lang.Long] {

  override def read(reader: Reader): java.lang.Long =
    reader.peek match {
      case TokenType.Number =>
        java.lang.Long.valueOf(reader.readLong())

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type Number, not $actual when reading Long value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: java.lang.Long, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeLong(value.longValue)
    }

}
