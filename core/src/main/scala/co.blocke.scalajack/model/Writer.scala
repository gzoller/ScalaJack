package co.blocke.scalajack
package model

import scala.reflect.runtime.universe._
import scala.collection.Map
import scala.collection.mutable
import scala.collection.immutable.ListMap
import ClassHelper.ExtraFieldValue

trait Writer[WIRE] {
  def writeArray[Elem](
      t:               Iterable[Elem],
      elemTypeAdapter: TypeAdapter[Elem],
      out:             mutable.Builder[WIRE, WIRE]): Unit
  def writeBigInt(t: BigInt, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeBoolean(t: Boolean, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeDecimal(t: BigDecimal, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeDouble(t: Double, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeInt(t: Int, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeLong(t: Long, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeMap[Key, Value, To](
      t:                Map[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              mutable.Builder[WIRE, WIRE]): Unit
  def writeNull(out: mutable.Builder[WIRE, WIRE]): Unit
  def writeObject[T](
      t:                  T,
      orderedFieldNames:  List[String],
      fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
      out:                mutable.Builder[WIRE, WIRE],
      extras:             List[(String, ExtraFieldValue[_])]                = List.empty[(String, ExtraFieldValue[_])]
  ): Unit
  def writeString(t: String, out: mutable.Builder[WIRE, WIRE]): Unit
  def writeRawString(t: String, out: mutable.Builder[WIRE, WIRE]): Unit // i.e. no quotes for JSON
  def writeTuple(
      writeFns: List[(Writer[WIRE], mutable.Builder[WIRE, WIRE]) => Unit],
      out:      mutable.Builder[WIRE, WIRE]
  ): Unit
}
