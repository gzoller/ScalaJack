package co.blocke.scalajack
package msgpack

import TokenType._
import scala.math.BigInt
import java.nio.ByteBuffer
import java.lang.Number

class MsgPackReader(
    source:         Array[Byte],
    numberOfTokens: Int,
    tokenTypes:     Array[TokenType],
    tokenOffsets:   Array[Int],
    tokenLengths:   Array[Int]
) extends Reader {

  var position: Int = -1

  def showError(): String = ""

  // --- Unsigned conversions
  val bit8mask = 0xff
  val bit16mask = (bit8mask.toLong << 8) | bit8mask
  val bit32mask = (bit16mask << 16) | bit16mask
  val bit64mask = (BigInt(bit32mask) << 32) | bit32mask

  private def toUnsignedByte(num: Byte): Short = {
    val aNum: Long = bit8mask & num.toLong
    aNum.toShort
  }
  private def toUnsignedShort(num: Short): Int = {
    val aNum: Long = bit16mask & num.toLong
    aNum.toInt
  }
  private def toUnsignedInt(num: Int): Long = {
    val aNum: Long = bit32mask & num.toLong
    aNum
  }
  private def toUnsignedLong(num: Long): BigInt = {
    val aNum: BigInt = bit64mask & BigInt(num)
    aNum
  }

  def captureValue(): Any = ???
  def peek = tokenTypes(position + 1)

  def read(expected: TokenType): Unit = {
    position += 1

    val actual = tokenTypes(position)
    if (actual != expected) {
      throw new IllegalStateException(s"Expected token of type $expected, not $actual\n" + showError())
    }
  }

  def read(): TokenType = {
    position += 1
    tokenTypes(position)
  }

  def readString(): String = {
    read(expected = TokenType.String)
    tokenText
  }

  override def readNumber(forJava: Boolean = false): java.lang.Number = {
    read(expected = TokenType.Number)
    readRawNumber()
  }

  private def readRawNumber(): Number =
    source(tokenOffsets(position)) match {
      // --- Unsigned Integers
      case b if ((b >> 7) == 0) => // unsigned 5-bit
        (b & 127).toInt
      case b if (b == 0xcc.toByte) => // unsigned 8-bit
        toUnsignedByte(source(tokenOffsets(position) + 1)).toInt
      case b if (b == 0xcd.toByte) => // unsigned 16-bit
        val num: Short = ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(tokenOffsets(position) + 1), source(tokenOffsets(position) + 2))).getInt.toShort
        toUnsignedShort(num) // Int
      case b if (b == 0xce.toByte) => // unsigned 32-bit
        val num: Int = ByteBuffer.wrap(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 5)).getInt
        toUnsignedInt(num) // Long
      case b if (b == 0xcf.toByte) => // unsigned 64-bit
        val num: Long = BigInt(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 9)).toLong
        toUnsignedLong(num) // BigInt

      // --- Signed Integers
      case b if (b == 0xd0.toByte) => // signed 8-bit
        source(tokenOffsets(position) + 1)
      case b if (b == 0xd1.toByte) => // signed 16-bit
        if (source(tokenOffsets(position) + 1) >> 7 != 0)
          ByteBuffer.wrap(Array(0xFF.toByte, 0xFF.toByte, source(tokenOffsets(position) + 1), source(tokenOffsets(position) + 2))).getInt
        else
          ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(tokenOffsets(position) + 1), source(tokenOffsets(position) + 2))).getInt
      case b if (b == 0xd2.toByte) => // signed 32-bit
        ByteBuffer.wrap(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 5)).getInt
      case b if (b == 0xd3.toByte) => // signed 64-bit
        BigInt(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 9)).toLong

      // --- Float & Double
      case b if (b == 0xca.toByte) => // signed 32-bit
        ByteBuffer.wrap(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 5)).getFloat
      case b if (b == 0xcb.toByte) => // signed 64-bit
        ByteBuffer.wrap(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 9)).getDouble
      // val bytes = ByteBuffer.wrap(source.slice(tokenOffsets(position + 1), tokenOffsets(position + 9)))
      // val scale = bytes.getInt()
      // val bibytes: Array[Byte] = new Array[Byte](bytes.remaining())
      // bytes.get(bibytes)
      // val bi = BigInt(bibytes)
      // BigDecimal(bi, scale)
    }

  override def readByte(): Byte = {
    read(expected = TokenType.Number)
    readRawNumber().byteValue
  }

  override def readShort(): Short = {
    read(expected = TokenType.Number)
    readRawNumber.intValue.toShort
  }

  override def readInt(): Int = {
    read(expected = TokenType.Number)
    readRawNumber.intValue
  }

  override def readLong(): Long = {
    read(expected = TokenType.Number)
    readRawNumber.longValue
  }

  override def readFloat(): Float = {
    read(expected = TokenType.Number)
    readRawNumber.floatValue
  }

  override def readDouble(): Double = {
    read(expected = TokenType.Number)
    readRawNumber.doubleValue
  }

  def tokenLengthAt(position: Int): Int = tokenLengths(position)
  def tokenOffsetAt(position: Int): Int = tokenOffsets(position)

  def tokenText: String = tokenTypes(position) match {
    case TokenType.String => source(tokenOffsets(position)) match {
      case b if (((b & 224) >> 5) == 5) => // 101XXXXX 
        val strlen = (b & 31) // mask = 31 = 11111
        new String(source, tokenOffsets(position) + 1, strlen, java.nio.charset.StandardCharsets.UTF_8)
      case b if (b == 0xd9.toByte) =>
        val strlen = (source(tokenOffsets(position) + 1).toInt)
        new String(source, tokenOffsets(position) + 2, strlen, java.nio.charset.StandardCharsets.UTF_8)
      case b if (b == 0xda.toByte) =>
        val strlen = ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(tokenOffsets(position) + 1), source(tokenOffsets(position) + 2))).getInt
        new String(source, tokenOffsets(position) + 3, strlen, java.nio.charset.StandardCharsets.UTF_8)
      case b if (b == 0xdb) =>
        val strlen = ByteBuffer.wrap(source.slice(tokenOffsets(position) + 1, tokenOffsets(position) + 5)).getInt
        new String(source, tokenOffsets(position) + 5, strlen, java.nio.charset.StandardCharsets.UTF_8)
    }
    case t => throw new IllegalStateException(s"Expected token of type String, not $t\n" + showError())
  }
}
