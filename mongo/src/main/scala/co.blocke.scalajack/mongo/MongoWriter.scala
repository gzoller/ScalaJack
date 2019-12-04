package co.blocke.scalajack
package mongo

import model._
import ClassHelper.ExtraFieldValue
import compat.BsonBuilder

import scala.collection.Map
import scala.collection.mutable
import org.bson._
import org.bson.types.Decimal128

case class MongoWriter(anyTypeAdapter: TypeAdapter[Any])
  extends Writer[BsonValue] {

  def writeBigInt(t: BigInt, out: mutable.Builder[BsonValue, BsonValue]): Unit =
    throw new ScalaJackError(
      "BigInt is currently an unsupported datatype for MongoDB serialization"
    )
  def writeBoolean(
      t:   Boolean,
      out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonBoolean(t)
  def writeDecimal(
      t:   BigDecimal,
      out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonDecimal128(new Decimal128(t.bigDecimal))
  def writeDouble(t: Double, out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonDouble(t)
  def writeInt(t: Int, out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonInt32(t)
  def writeLong(t: Long, out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonInt64(t)
  def writeNull(out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonNull()
  def writeString(t: String, out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonString(t)
  def writeRawString(
      t:   String,
      out: mutable.Builder[BsonValue, BsonValue]): Unit =
    out += new BsonString(t)

  def writeArray[Elem](
      t:               Iterable[Elem],
      elemTypeAdapter: TypeAdapter[Elem],
      out:             mutable.Builder[BsonValue, BsonValue]): Unit =
    t match {
      case null => out += new BsonNull()
      case a =>
        val array = new BsonArray()
        val builder = BsonBuilder()
        val iter = a.iterator
        while (iter.hasNext) {
          elemTypeAdapter.write[BsonValue](iter.next, this, builder)
          array.add(builder.result())
          builder.clear()
        }
        out += array
    }

  def writeMap[Key, Value, To](
      t:                Map[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              mutable.Builder[BsonValue, BsonValue]
  ): Unit = t match {
    case null => out += new BsonNull()
    case daMap =>
      val doc = new BsonDocument()
      val keyBuilder = BsonBuilder()
      val valueBuilder = BsonBuilder()
      daMap.foreach {
        case (key, value) =>
          if (key == null)
            throw new ScalaJackError("Map keys cannot be null.")
          keyTypeAdapter.write(key, this, keyBuilder)
          valueTypeAdapter.write(value, this, valueBuilder)
          doc.append(
            keyBuilder.result().asString.getValue,
            valueBuilder.result()
          )
          keyBuilder.clear()
          valueBuilder.clear()
      }
      out += doc
  }

  def writeTuple[T](
      t:        T,
      writeFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]],
      out:      mutable.Builder[BsonValue, BsonValue]
  ): Unit = {
    val array = new BsonArray()
    val builder = BsonBuilder()
    writeFns.foreach { f =>
      f.write(t, this, builder)
      array.add(builder.result())
      builder.clear()
    }
    out += array
  }

  @inline private def writeFields(
      fields: List[(String, Any, TypeAdapter[Any])],
      doc:    BsonDocument): Unit = {
    for ((label, value, valueTypeAdapter) <- fields)
      if (value != None) {
        val builder = BsonBuilder()
        valueTypeAdapter.write(value, this, builder)
        doc.append(label, builder.result())
      }
  }

  def writeObject[T](
      t:                  T,
      orderedFieldNames:  List[String],
      fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
      out:                mutable.Builder[BsonValue, BsonValue],
      extras:             List[(String, ExtraFieldValue[_])]                = List.empty[(String, ExtraFieldValue[_])]
  ): Unit =
    if (t == null)
      out += new BsonNull()
    else {
      val doc = new BsonDocument()
      writeFields(
        extras.map(
          e =>
            (
              e._1,
              e._2.value,
              e._2.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]
            )
        ),
        doc
      )

      // Logic to handle _id (@DBKey)
      val (dbkeys, theRest) =
        fieldMembersByName.partition(_._2.dbKeyIndex.isDefined)
      if (dbkeys.isEmpty) // no @DBKey fields
        writeFields(
          fieldMembersByName
            .map(f => (f._1, f._2.valueIn(t), f._2.valueTypeAdapter))
            .toList,
          doc
        )
      else {
        if (dbkeys.size == 1) {
          val toWrite = List(
            (
              ID_FIELD,
              fieldMembersByName(dbkeys.head._1).valueIn(t),
              fieldMembersByName(dbkeys.head._1).valueTypeAdapter
            )
          )
          writeFields(toWrite, doc)
        } else {
          val idDoc = new BsonDocument()
          val toWrite = dbkeys
            .map(e => (e._1, e._2.valueIn(t), e._2.valueTypeAdapter))
            .toList
          writeFields(toWrite, idDoc)
          doc.append(ID_FIELD, idDoc)
        }
        val toWrite = theRest
          .map(e => (e._1, e._2.valueIn(t), e._2.valueTypeAdapter))
          .toList
        writeFields(toWrite, doc)
      }

      t match {
        case sjc: SJCapture =>
          import scala.collection.JavaConverters._
          sjc.captured.asScala.foreach {
            case (label, capturedValue) =>
              doc.append(label, capturedValue.asInstanceOf[BsonValue])
          }
        case _ =>
      }
      out += doc
    }
}
