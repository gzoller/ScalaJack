package co.blocke.scalajack

trait Writer {

  def beginObject(): Unit

  def endObject(): Unit

  def beginArray(): Unit

  def endArray(): Unit

  def writeJsonValue[J](json: J)(implicit ops: JsonOps[J]): Unit = {
    json match {
      case JsonArray(x) =>
        beginArray()
        ops.foreachArrayElement(x.asInstanceOf[ops.ArrayElements], { (index, element) =>
          writeJsonValue(element)
        })
        endArray()

      case JsonBoolean(x) =>
        writeBoolean(x)

      case JsonDecimal(x) =>
        writeRawValue(x.toString)

      case JsonDouble(x) =>
        writeDouble(x)

      case JsonInt(x) =>
        writeRawValue(x.toString)

      case JsonLong(x) =>
        writeLong(x)

      case JsonNull() =>
        writeNull()

      case JsonObject(x) =>
        beginObject()
        ops.foreachObjectField(x.asInstanceOf[ops.ObjectFields], { (name, value) =>
          writeString(name)
          writeJsonValue(value)
        })
        endObject()

      case JsonString(x) =>
        writeString(x)
    }
  }

  def writeRawValue(raw: String): Unit = {
    val charArray = raw.toCharArray
    writeRawValue(charArray, 0, charArray.length)
  }

  def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit

  def writeNothing(): Unit

  def writeString(string: String): Unit

  def writeByte(value: Byte): Unit

  def writeShort(value: Short): Unit

  def writeInt(value: Int): Unit

  def writeFloat(value: Float): Unit

  def writeDouble(value: Double): Unit

  def writeLong(value: Long): Unit

  def writeBoolean(value: Boolean): Unit

  def writeFalse(): Unit

  def writeTrue(): Unit

  def writeNull(): Unit

  def writeChar(value: Char): Unit

}

trait ForwardingWriter extends Writer {

  def delegate: Writer

  override def beginObject(): Unit = delegate.beginObject()

  override def endObject(): Unit = delegate.endObject()

  override def beginArray(): Unit = delegate.beginArray()

  override def endArray(): Unit = delegate.endArray()

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = delegate.writeRawValue(source, offset, length)

  override def writeNothing(): Unit = delegate.writeNothing()

  override def writeString(string: String): Unit = delegate.writeString(string)

  override def writeByte(value: Byte): Unit = delegate.writeByte(value)

  override def writeShort(value: Short): Unit = delegate.writeShort(value)

  override def writeInt(value: Int): Unit = delegate.writeInt(value)

  override def writeFloat(value: Float): Unit = delegate.writeFloat(value)

  override def writeDouble(value: Double): Unit = delegate.writeDouble(value)

  override def writeLong(value: Long): Unit = delegate.writeLong(value)

  override def writeBoolean(value: Boolean): Unit = delegate.writeBoolean(value)

  override def writeFalse(): Unit = delegate.writeFalse()

  override def writeTrue(): Unit = delegate.writeTrue()

  override def writeNull(): Unit = delegate.writeNull()

  override def writeChar(value: Char): Unit = delegate.writeChar(value)

}
