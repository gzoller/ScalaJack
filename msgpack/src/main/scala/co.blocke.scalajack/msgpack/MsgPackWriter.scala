package co.blocke.scalajack
package msgpack

import org.msgpack.packer.MessagePackPacker
import org.msgpack.MessagePack
import java.io.OutputStream

case class MsgPackWriter(msgpack: MessagePack, out: OutputStream) extends MsgPackWriterLike

trait MsgPackWriterLike extends Writer {
  val msgpack: MessagePack
  val out: OutputStream

  private lazy val packer = new PackerWithDump(msgpack, out)

  def beginObject(): Unit = {} // not used
  def beginObject(size: Int): Unit = packer.writeMapBegin(size)

  def endObject(): Unit = packer.writeMapEnd()

  def beginArray(): Unit = {}
  def beginArray(size: Int): Unit = packer.writeArrayBegin(size)

  def endArray(): Unit = packer.writeArrayEnd()

  def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = packer.write(new String(source, offset, length))

  def writeNothing(): Unit = {}

  def writeString(string: String): Unit = packer.write(string)

  def writeByte(value: Byte): Unit = packer.write(value)

  def writeShort(value: Short): Unit = packer.write(value)

  def writeInt(value: Int): Unit = packer.write(value)

  def writeFloat(value: Float): Unit = packer.write(value)

  def writeDouble(value: Double): Unit = packer.write(value)

  def writeLong(value: Long): Unit = packer.write(value)

  def writeBoolean(value: Boolean): Unit = packer.write(value)

  def writeFalse(): Unit = packer.write(false)

  def writeTrue(): Unit = packer.write(true)

  def writeNull(): Unit = packer.writeNil()

  def writeChar(value: Char): Unit = packer.write(value.toString.getBytes)

  def writeByteArray(value: Array[Byte]): Unit = packer.write(value)

  // Like writeByteArray but don't do any msgpack prefix
  def dumpBytes(value: Array[Byte]): Unit = packer.dumpBytes(value)
}