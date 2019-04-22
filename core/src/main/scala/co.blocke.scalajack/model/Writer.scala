package co.blocke.scalajack
package model

import scala.collection.Map
import scala.collection.mutable.Builder
import scala.collection.immutable.ListMap
import ClassHelper.ExtraFieldValue

trait Writer[WIRE] extends Transceiver[WIRE] {

  def writeArray[Elem](t: Iterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[WIRE, WIRE]): Unit
  def writeBigInt(t: BigInt, out: Builder[WIRE, WIRE]): Unit
  def writeBoolean(t: Boolean, out: Builder[WIRE, WIRE]): Unit
  def writeDecimal(t: BigDecimal, out: Builder[WIRE, WIRE]): Unit
  def writeDouble(t: Double, out: Builder[WIRE, WIRE]): Unit
  def writeInt(t: Int, out: Builder[WIRE, WIRE]): Unit
  def writeLong(t: Long, out: Builder[WIRE, WIRE]): Unit
  def writeMap[Key, Value, To](t: Map[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: Builder[WIRE, WIRE])(implicit keyTT: TypeTag[Key]): Unit
  def writeNull(out: Builder[WIRE, WIRE]): Unit
  def writeObject[T](
      t:            T,
      fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
      out:          Builder[WIRE, WIRE],
      extras:       List[(String, ExtraFieldValue[_])]                    = List.empty[(String, ExtraFieldValue[_])]): Unit
  def writeString(t: String, out: Builder[WIRE, WIRE]): Unit
  def writeTuple(writeFns: List[(Writer[WIRE], Builder[WIRE, WIRE]) => Unit], out: Builder[WIRE, WIRE]): Unit
}
