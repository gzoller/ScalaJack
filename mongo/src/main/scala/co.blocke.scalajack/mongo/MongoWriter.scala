package co.blocke.scalajack
package mongo

import model._
import ClassHelper.ExtraFieldValue

import scala.collection.Map
import scala.collection.immutable.ListMap
import scala.collection.mutable.Builder
import org.mongodb.scala.bson._

case class MongoWriter(jackFlavor: JackFlavor[BsonValue]) extends Writer[BsonValue] {

  def writeBigInt(t: BigInt, out: Builder[BsonValue, BsonValue]): Unit = throw new SJError("BigInt is currently an unsupported datatype for MongoDB serialization")
  def writeBoolean(t: Boolean, out: Builder[BsonValue, BsonValue]): Unit = out += new BsonBoolean(t)
  def writeDecimal(t: BigDecimal, out: Builder[BsonValue, BsonValue]): Unit = out += BsonDecimal128(t)
  def writeDouble(t: Double, out: Builder[BsonValue, BsonValue]): Unit = out += new BsonDouble(t)
  def writeInt(t: Int, out: Builder[BsonValue, BsonValue]): Unit = out += new BsonInt32(t)
  def writeLong(t: Long, out: Builder[BsonValue, BsonValue]): Unit = out += new BsonInt64(t)
  def writeNull(out: Builder[BsonValue, BsonValue]): Unit = out += new BsonNull()
  def writeString(t: String, out: Builder[BsonValue, BsonValue]): Unit = out += new BsonString(t)

  def writeArray[Elem](t: Iterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[BsonValue, BsonValue]): Unit = t match {
    case null => out += new BsonNull()
    case a =>
      val array = BsonArray()
      val builder = BsonBuilder()
      val iter = a.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write[BsonValue](iter.next, this, builder, false)
        array.add(builder.result())
        builder.clear
      }
      out += array
  }

  def writeMap[Key, Value, To](t: Map[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: Builder[BsonValue, BsonValue])(implicit keyTT: TypeTag[Key]): Unit = t match {
    case null => out += BsonNull()
    case daMap =>
      val doc = BsonDocument()
      val keyBuilder = BsonBuilder()
      val valueBuilder = BsonBuilder()
      daMap.foreach {
        case (key, value) =>
          if (key == null)
            throw new SJError("Map keys cannot be null.")
          keyTypeAdapter.write(key, this, keyBuilder, true)
          valueTypeAdapter.write(value, this, valueBuilder, false)
          doc.append(keyBuilder.result().asString.getValue, valueBuilder.result())
          keyBuilder.clear
          valueBuilder.clear
      }
      out += doc
  }

  def writeTuple(writeFns: List[(Writer[BsonValue], Builder[BsonValue, BsonValue]) => Unit], out: Builder[BsonValue, BsonValue]): Unit = {
    val array = BsonArray()
    val builder = BsonBuilder()
    writeFns.foreach { f =>
      f(this, builder)
      array.add(builder.result())
      builder.clear
    }
    out += array
  }

  @inline private def writeFields(fields: List[(String, Any, TypeAdapter[Any])], doc: BsonDocument): Unit = {
    for ((label, value, valueTypeAdapter) <- fields)
      if (value != None) {
        val builder = BsonBuilder()
        valueTypeAdapter.write(value, this, builder, false)
        doc.append(label, builder.result())
      }
  }

  def writeObject[T](
      t:            T,
      fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
      out:          Builder[BsonValue, BsonValue],
      extras:       List[(String, ExtraFieldValue[_])]                    = List.empty[(String, ExtraFieldValue[_])]): Unit = {
    if (t == null) {
      out += BsonNull()
    } else {
      val doc = BsonDocument()
      writeFields(extras.map(e => (e._1, e._2.value, e._2.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]])), doc)
      writeFields(fieldMembers.map(f => (f._1, f._2.valueIn(t), f._2.valueTypeAdapter)).toList, doc)
      t match {
        case sjc: SJCapture =>
          writeFields(sjc.captured.map(c => (c._1, c._2, jackFlavor.anyTypeAdapter)).toList, doc)
        case _ =>
      }
      out += doc
    }
  }
}
