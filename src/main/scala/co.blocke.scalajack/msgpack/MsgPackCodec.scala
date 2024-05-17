package co.blocke.scalajack
package msgpack

import org.msgpack.core.{MessagePacker, MessageUnpacker}

trait MsgPackCodec[A] {
  def encodeValue(in: A, out: MessagePacker): Unit
  def decodeValue(in: MessageUnpacker): A
}
