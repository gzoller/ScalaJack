package co.blocke.scalajack
package msgpack

// import org.msgpack.packer.MessagePackPacker
// import java.io.InputStream
import java.nio.ByteBuffer
import TokenType._

case class MsgPackTokenizer(val capacity: Int = 1024) {

  val tokenTypes = new Array[TokenType](capacity)
  val tokenOffsets = new Array[Int](capacity)
  val tokenLengths = new Array[Int](capacity)
  var numberOfTokens = 0

  val stack = Array.fill(capacity)(0)
  val objContextStack = Array.fill(capacity)(false)
  var stackPtr = -1

  @inline def stackPush(num: Int, isObjectContext: Boolean) = {
    stackPtr += 1
    stack(stackPtr) = num
    objContextStack(stackPtr) = isObjectContext
  }

  @inline def stackDecrement(position: Int): Unit = if (stackPtr >= 0) {
    stack(stackPtr) -= 1
    if (stack(stackPtr) == 0) {
      if (objContextStack(stackPtr))
        appendToken(TokenType.EndObject, position, 0)
      else
        appendToken(TokenType.EndArray, position, 0)
      stackPtr -= 1
      stackDecrement(position)
    } else if (stack(stackPtr) < 0)
      stack(stackPtr) = 0
  }

  @inline def appendToken(tokenType: TokenType, tokenOffset: Int, tokenLength: Int): Unit = {
    tokenTypes(numberOfTokens) = tokenType
    tokenOffsets(numberOfTokens) = tokenOffset
    tokenLengths(numberOfTokens) = tokenLength
    numberOfTokens += 1
  }

  def tokenize(source: Array[Byte]) = {
    var pos = 0
    val maxpos = source.length

    while (pos < maxpos) {
      // println(s"::: $pos ::: ${source(pos).toHexString}")
      source(pos) match {
        // numbers
        case b if ((b >> 7) == 0) =>
          appendToken(TokenType.Number, pos, 1)
          pos += 1
          stackDecrement(pos)
        case b if (b == 0xcc.toByte || b == 0xd0.toByte) =>
          appendToken(TokenType.Number, pos, 2)
          pos += 2
          stackDecrement(pos)
        case b if (b == 0xcd.toByte || b == 0xd1.toByte) =>
          appendToken(TokenType.Number, pos, 3)
          pos += 3
          stackDecrement(pos)
        case b if (b == 0xce.toByte || b == 0xd2.toByte || b == 0xca.toByte) =>
          appendToken(TokenType.Number, pos, 5)
          pos += 5
          stackDecrement(pos)
        case b if (b == 0xcf.toByte || b == 0xd3.toByte || b == 0xcb.toByte) =>
          appendToken(TokenType.Number, pos, 9)
          pos += 9
          stackDecrement(pos)

        // String
        case b if (((b & 224) >> 5) == 5) => // 101XXXXX 
          val strlen = (b & 31) + 1 // mask = 31 = 11111
          appendToken(TokenType.String, pos, strlen)
          pos += strlen
          stackDecrement(pos)
        case b if (b == 0xd9.toByte) =>
          val strlen = (source(pos + 1).toInt) + 2
          appendToken(TokenType.String, pos, strlen)
          pos += strlen
          stackDecrement(pos)
        case b if (b == 0xda.toByte) =>
          val strlen = ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(pos + 1), source(pos + 2))).getInt + 3
          appendToken(TokenType.String, pos, strlen)
          pos += strlen
          stackDecrement(pos.toByte)
        case b if (b == 0xdb) =>
          val strlen = ByteBuffer.wrap(source.slice(pos + 1, pos + 5)).getInt + 5
          appendToken(TokenType.String, pos, strlen)
          pos += strlen
          stackDecrement(pos)

        // nil
        case b if (b == 0xc0.toByte) =>
          appendToken(TokenType.Null, pos, 1)
          pos += 1
          stackDecrement(pos)

        // false
        case b if (b == 0xc2.toByte) =>
          appendToken(TokenType.False, pos, 1)
          pos += 1
          stackDecrement(pos)

        // true
        case b if (b == 0xc3.toByte) =>
          appendToken(TokenType.True, pos, 1)
          pos += 1
          stackDecrement(pos)

        // Map
        case b if (((b & 240) >> 4) == 8) => // 1000XXXX
          appendToken(TokenType.BeginObject, pos, 0)
          val numKVPairs = b & 15 // 15 = 1111 to use as a mask
          stackPush(numKVPairs * 2, true)
          pos += 1
        case b if (b == 0xde.toByte) =>
          appendToken(TokenType.BeginObject, pos, 0)
          val numKVPairs = ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(pos + 1), source(pos + 2))).getInt
          stackPush(numKVPairs * 2, true)
          pos += 3
        case b if (b == 0xdf.toByte) =>
          appendToken(TokenType.BeginObject, pos, 0)
          val numKVPairs = ByteBuffer.wrap(source.slice(pos + 1, pos + 5)).getInt
          stackPush(numKVPairs * 2, true)
          pos += 5

        // Array
        case b if (((b & 240) >> 4) == 9) => // 1000XXXX
          appendToken(TokenType.BeginArray, pos, 0)
          val numElems = b & 15 // 15 = 1111 to use as a mask
          stackPush(numElems, false)
          pos += 1
        case b if (b == 0xdc.toByte) =>
          appendToken(TokenType.BeginArray, pos, 0)
          val numElems = ByteBuffer.wrap(Array(0.toByte, 0.toByte, source(pos + 1), source(pos + 2))).getInt
          stackPush(numElems, false)
          pos += 3
        case b if (b == 0xdd.toByte) =>
          appendToken(TokenType.BeginArray, pos, 0)
          val numElems = ByteBuffer.wrap(source.slice(pos + 1, pos + 5)).getInt
          stackPush(numElems, false)
          pos += 5
      }
    }
    // println(tokenTypes.take(numberOfTokens).toList)
    new MsgPackReader(source, numberOfTokens, tokenTypes, tokenOffsets, tokenLengths)
  }
}