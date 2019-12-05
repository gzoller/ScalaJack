package co.blocke.scalajack
package json4s

import model._
import model.Writer
import ClassHelper.ExtraFieldValue
import co.blocke.scalajack.compat.JValueBuilder

import scala.collection.{ Map, mutable }
import org.json4s._

case class Json4sWriter() extends Writer[JValue] {

  def writeArray[Elem](
      t:               Iterable[Elem],
      elemTypeAdapter: TypeAdapter[Elem],
      out:             collection.mutable.Builder[JValue, JValue]): Unit =
    t match {
      case null => out += JNull
      case a =>
        var arr = JArray(List.empty[JValue])
        val outBuf = JValueBuilder()
        a.iterator.foreach { item =>
          outBuf.clear()
          elemTypeAdapter.write(item, this, outBuf)
          arr = JArray(arr.arr :+ outBuf.result)
        }
        out += arr
    }

  def writeRaw(t: Any, out: mutable.Builder[JValue, JValue]): Unit =
    out += t.asInstanceOf[JValue]

  def writeBigInt(
      t:   BigInt,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JInt(t)

  def writeBoolean(
      t:   Boolean,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JBool(t)

  def writeDecimal(
      t:   BigDecimal,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    t match {
      case null => out += JNull
      case d    => out += JDecimal(d)
    }

  def writeDouble(
      t:   Double,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JDouble(t)

  def writeInt(t: Int, out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JInt(t)

  def writeLong(
      t:   Long,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JLong(t)

  def writeMap[Key, Value, To](
      t:                Map[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              mutable.Builder[JValue, JValue]): Unit =
    t match {
      case null => out += JNull
      case daMap =>
        val outBuf = JValueBuilder()
        val outMap = daMap.map {
          case (key, value) =>
            if (key == null)
              throw new ScalaJackError("Map keys cannot be null.")
            outBuf.clear()
            keyTypeAdapter.write(key, this, outBuf)
            val k = outBuf.result().values.toString
            outBuf.clear()
            valueTypeAdapter.write(value, this, outBuf)
            k -> outBuf.result
        }.toList
        out += JObject(outMap)
    }

  def writeString(
      t:   String,
      out: collection.mutable.Builder[JValue, JValue]): Unit =
    t match {
      case null      => out += JNull
      case _: String => out += JString(t)
    }

  def writeNull(out: collection.mutable.Builder[JValue, JValue]): Unit =
    out += JNull

  @inline private def writeFields(
      fields: List[(String, Any, TypeAdapter[Any])]
  ): Map[String, JValue] = {
    val outBuf = JValueBuilder()
    fields.collect {
      case (label, value, valueTypeAdapter) if value != None =>
        outBuf.clear()
        valueTypeAdapter.write(value, this, outBuf)
        label -> outBuf.result()
    }.toMap
  }

  def writeObject[T](
      t:                  T,
      orderedFieldNames:  List[String],
      fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
      out:                mutable.Builder[JValue, JValue],
      extras:             List[(String, ExtraFieldValue[_])]                = List.empty[(String, ExtraFieldValue[_])]
  ): Unit =
    t match {
      case null => out += JNull
      case _ =>
        val extraFields = writeFields(
          extras.map(
            e =>
              (
                e._1,
                e._2.value,
                e._2.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]
              )
          )
        )
        val classFields = writeFields(orderedFieldNames.map { orn =>
          val oneField = fieldMembersByName(orn)
          (orn, oneField.valueIn(t), oneField.valueTypeAdapter)
        })
        val captureFields = t match {
          case sjc: SJCapture =>
            import scala.collection.JavaConverters._
            sjc.captured.asScala.asInstanceOf[Map[String, JValue]]
          case _ => Map.empty[String, JValue]
        }

        out += JObject((extraFields ++ classFields ++ captureFields).toList)
    }

  def writeTuple[T](
      t:        T,
      writeFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]],
      out:      mutable.Builder[JValue, JValue]
  ): Unit = {
    var arr = JArray(List.empty[JValue])
    val outBuf = JValueBuilder()
    writeFns.foreach { f =>
      outBuf.clear()
      f.write(t, this, outBuf)
      arr = JArray(arr.arr :+ outBuf.result)
    }
    out += arr
  }
}
