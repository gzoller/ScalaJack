package co.blocke.scalajack
package csv

import java.lang.UnsupportedOperationException

class StringCSVWriter() extends Writer {

  private val builder = scala.collection.mutable.ListBuffer.empty[String]
  private var level = -1

  def csvString: String = builder.mkString(",")

  def beginObject(): Unit = if (level < 0) level = 0 else throw new UnsupportedOperationException("Writing a nested object is not supported in CSV format")

  def endObject(): Unit = if (level != 0) throw new UnsupportedOperationException("Writing a nested object is not supported in CSV format")

  def beginArray(): Unit = throw new UnsupportedOperationException("Writing a nested array is not supported in CSV format")

  def endArray(): Unit = throw new UnsupportedOperationException("Writing a nested array is not supported in CSV format")

  override def writeRawValue(raw: String): Unit = builder += raw

  def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = throw new UnsupportedOperationException("Raw array otuput not supported for CSV format")

  def writeNothing(): Unit = builder += ""

  def writeString(string: String): Unit = {
    string match {
      case s if (s.contains('"')) ⇒ builder += '"' + s.replaceAllLiterally("\"", "\"\"") + '"'
      case s if (s.contains(',')) ⇒ builder += '"' + s + '"'
      case s if (s.isEmpty)       ⇒ builder += "\"\""
      case s                      ⇒ builder += s
    }
  }

  def writeByte(value: Byte): Unit = builder += value.toString

  def writeShort(value: Short): Unit = builder += value.toString

  def writeInt(value: Int): Unit = builder += value.toString

  def writeFloat(value: Float): Unit = builder += value.toString

  def writeDouble(value: Double): Unit = builder += value.toString

  def writeLong(value: Long): Unit = builder += value.toString

  def writeBoolean(value: Boolean): Unit = builder += value.toString

  def writeFalse(): Unit = builder += "false"

  def writeTrue(): Unit = builder += "true"

  def writeNull(): Unit = builder += ""

  def writeChar(value: Char): Unit = builder += value.toString

}