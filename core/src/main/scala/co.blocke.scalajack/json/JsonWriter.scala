package co.blocke.scalajack
package json

import model._
import ClassHelper.ExtraFieldValue

import scala.collection.{ GenIterable, GenMap }
import scala.collection.immutable.ListMap
import scala.collection.mutable.Builder

case class JsonWriter(jackFlavor: JackFlavor[String]) extends Writer[String] {

  @inline def addString(s: String, out: Builder[String, String]): Unit = out += s //s.toCharArray.foreach(c => out += c)

  def writeArray[Elem](t: GenIterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[String, String]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += "["
      val iter = a.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this, out, false)
        if (iter.hasNext)
          out += ","
      }
      out += "]"
  }

  def writeBigInt(t: BigInt, out: Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeBoolean(t: Boolean, out: Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeDecimal(t: BigDecimal, out: Builder[String, String]): Unit = t match {
    case null => addString("null", out)
    case s    => addString(s.toString, out)
  }

  def writeDouble(t: Double, out: Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeInt(t: Int, out: Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeLong(t: Long, out: Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeMap[Key, Value, To](
      t:                GenMap[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              Builder[String, String])(implicit keyTT: TypeTag[Key]): Unit = t match {
    case null => addString("null", out)
    case daMap =>
      out += "{"
      var first = true
      daMap.foreach {
        case (key, value) =>
          if (first)
            first = false
          else
            out += ","
          if (key == null)
            throw new SJError("Map keys cannot be null.")
          keyTypeAdapter.write(key, this, out, true)
          out += ":"
          valueTypeAdapter.write(value, this, out, false)
      }
      out += "}"
  }

  def writeRawString(t: String, out: Builder[String, String]): Unit = addString(t, out)

  def writeString(t: String, out: Builder[String, String]): Unit = t match {
    case null => addString("null", out)
    case _: String =>
      out += "\""
      var i = 0
      val length = t.length
      val chars = t.toCharArray

      while (i < length) {
        chars(i) match {
          case '"'  => addString("""\"""", out)
          case '\\' => addString("""\\""", out)
          case '\b' => addString("""\b""", out)
          case '\f' => addString("""\f""", out)
          case '\n' => addString("""\n""", out)
          case '\r' => addString("""\r""", out)
          case '\t' => addString("""\t""", out)
          case ch if ch < 32 || ch >= 128 =>
            addString("""\""" + "u" + "%04x".format(ch.toInt), out)
          case c => out += c.toString
        }

        i += 1
      }
      out += "\""
  }

  def writeNull(out: Builder[String, String]): Unit = addString("null", out)

  @inline private def writeFields(isFirst: Boolean, fields: List[(String, Any, TypeAdapter[Any])], out: Builder[String, String]): Boolean = {
    var first = isFirst
    for ((label, value, valueTypeAdapter) <- fields)
      if (value != None) {
        if (first)
          first = false
        else
          out += ","
        writeString(label, out)
        out += ":"
        valueTypeAdapter.write(value, this, out, false)
      }
    first
  }

  def writeObject[T](
      t:            T,
      fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
      out:          Builder[String, String],
      extras:       List[(String, ExtraFieldValue[_])]): Unit = {
    if (t == null) {
      addString("null", out)
    } else {
      out += "{"
      val wasFirst = writeFields(true, extras.map(e => (e._1, e._2.value, e._2.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]])), out)
      val wasFirst2 = writeFields(wasFirst, fieldMembers.map(f => (f._1, f._2.valueIn(t), f._2.valueTypeAdapter)).toList, out)
      t match {
        case sjc: SJCapture =>
          writeFields(wasFirst2, sjc.captured.map(c => (c._1, c._2, jackFlavor.anyTypeAdapter)).toList, out)
        case _ =>
      }
      out += "}"
    }
  }

  def writeTuple(writeFns: List[(Writer[String], Builder[String, String]) => Unit], out: Builder[String, String]): Unit = {
    out += "["
    var first = true
    writeFns.map { f =>
      if (first)
        first = false
      else
        out += ","
      f(this, out)
    }
    out += "]"
  }
}
