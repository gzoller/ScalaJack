package co.blocke.scalajack
package model

import scala.collection.{ GenIterable, GenMap }
import scala.collection.mutable.Builder
import scala.collection.immutable.ListMap

//trait Builder[B] {
//  def append(b: B): _
//  def result(): B
//}

trait Writer[WIRE] {

  def writeArray[Elem](t: GenIterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[Any, WIRE]): Unit
  def writeBigInt(t: BigInt, out: Builder[Any, WIRE]): Unit
  def writeBoolean(t: Boolean, out: Builder[Any, WIRE]): Unit
  def writeDecimal(t: BigDecimal, out: Builder[Any, WIRE]): Unit
  def writeDouble(t: Double, out: Builder[Any, WIRE]): Unit
  def writeInt(t: Int, out: Builder[Any, WIRE]): Unit
  def writeLong(t: Long, out: Builder[Any, WIRE]): Unit
  def writeMap[Key, Value, To](t: GenMap[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: Builder[Any, WIRE]): Unit
  def writeNull(out: Builder[Any, WIRE]): Unit
  def writeObject[T](t: T, fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]], out: Builder[Any, WIRE]): Unit
  def writeRawString(t: String, out: Builder[Any, WIRE]): Unit
  def writeString(t: String, out: Builder[Any, WIRE]): Unit
}
