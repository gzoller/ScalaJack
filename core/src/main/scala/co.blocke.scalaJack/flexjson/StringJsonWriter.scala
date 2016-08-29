package co.blocke.scalajack.flexjson

class StringJsonWriter extends Writer {

  val builder = new StringBuilder

  def jsonString: String = builder.toString

  override def beginObject(): Unit =
    builder.append("{")

  override def endObject(): Unit =
    builder.append("}")

  override def beginArray(): Unit =
    builder.append("[")

  override def endArray(): Unit =
    builder.append("]")

  override def writeName(name: String): Unit =
    writeString(name)

  override def writeString(string: String): Unit =
    builder.append('"').append(string).append('"') // TODO escape values

  override def writeInt(value: Int): Unit =
    builder.append(value)

  override def writeNameSeparator(): Unit =
    builder.append(":")

  override def writeValueSeparator(): Unit =
    builder.append(",")

  override def writeFalse(): Unit =
    builder.append("false")

  override def writeTrue(): Unit =
    builder.append("true")

  override def writeNull(): Unit =
    builder.append("null")

  override def writeFloat(value: Float): Unit =
    builder.append(value)

  override def writeDouble(value: Double): Unit =
    builder.append(value)

  override def writeLong(value: Long): Unit =
    builder.append(value)

  override def writeChar(value: Char): Unit =
    builder.append(value)

  override def writeByte(value: Byte): Unit =
    builder.append(value)

  override def writeShort(value: Short): Unit =
    builder.append(value)

}
