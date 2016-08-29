package co.blocke.scalajack.flexjson

trait Writer {

  def beginObject(): Unit

  def endObject(): Unit

  def beginArray(): Unit

  def endArray(): Unit

  def writeName(name: String): Unit

  def writeNameSeparator(): Unit

  def writeValueSeparator(): Unit

  def writeString(string: String): Unit

  def writeByte(value: Byte): Unit

  def writeShort(value: Short): Unit

  def writeInt(value: Int): Unit

  def writeFloat(value: Float): Unit

  def writeDouble(value: Double): Unit

  def writeLong(value: Long): Unit

  def writeFalse(): Unit

  def writeTrue(): Unit

  def writeNull(): Unit

  def writeChar(value: Char): Unit

}
