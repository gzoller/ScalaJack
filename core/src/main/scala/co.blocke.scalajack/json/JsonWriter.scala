package co.blocke.scalajack
package json

import model._
import ClassHelper.ExtraFieldValue

import scala.collection.{ GenIterable, GenMap }
import scala.collection.immutable.ListMap
import scala.collection.mutable.Builder

trait JsonWriter extends Writer[String] {

  this: JsonTransciever =>

  @inline def addString(s: String, out: Builder[Any, String]): Unit = s.toCharArray.map(c => out += c)

  def writeArray[Elem](t: GenIterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += '['
      val iter = a.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this, out, false)
        if (iter.hasNext)
          out += ','
      }
      out += ']'
  }

  def writeBigInt(t: BigInt, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case s    => addString(s.toString, out)
  }

  def writeBoolean(t: Boolean, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeDecimal(t: BigDecimal, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case s    => addString(s.toString, out)
  }

  def writeDouble(t: Double, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeInt(t: Int, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeLong(t: Long, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeMap[Key, Value, To](
      t:                GenMap[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              Builder[Any, String])(implicit keyTT: TypeTag[Key]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += '{'
      val iter = a.iterator
      while (iter.hasNext) {
        val kv = iter.next
        if (kv._1 == null)
          throw new IllegalStateException("Map keys cannot be null.")
        keyTypeAdapter.write(kv._1, this, out, true)
        out += ':'
        valueTypeAdapter.write(kv._2, this, out, false)
        if (iter.hasNext)
          out += ','
      }
      out += '}'
  }

  def writeRawString(t: String, out: Builder[Any, String]): Unit = addString(t, out)

  def writeString(t: String, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case _: String =>
      out += '"'
      var i = 0
      val length = t.length
      val chars = t.toCharArray

      while (i < length) {
        chars(i) match {
          case '"'  => addString("""\"""", out)
          case ' '  => addString(" ", out)
          case '\\' => addString("""\\""", out)
          case '/'  => addString("""\/""", out)
          case '\b' => addString("""\b""", out)
          case '\f' => addString("""\f""", out)
          case '\n' => addString("""\n""", out)
          case '\r' => addString("""\r""", out)
          case '\t' => addString("""\t""", out)
          case ch if ch <= 32 || ch >= 128 =>
            addString("""\""" + "u" + "%04x".format(ch.toInt), out)
          case c => out += c
        }

        i += 1
      }
      out += '"'
  }

  def writeNull(out: Builder[Any, String]): Unit = addString("null", out)

  def writeObject[T](
      t:            T,
      fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
      out:          Builder[Any, String],
      extras:       List[(String, ExtraFieldValue[_])]): Unit = {
    if (t == null) {
      addString("null", out)
    } else {
      out += '{'
      var first = true
      for ((label, extraVal) <- extras) {
        if (first)
          first = false
        else
          out += ','
        writeString(label, out)
        out += ':'
        extraVal.write(this, out)
      }
      for ((memberName, member) <- fieldMembers) {
        val memberValue = member.valueIn(t)
        if (memberValue != None) {
          if (first)
            first = false
          else
            out += ','
          writeString(memberName, out)
          out += ':'
          member.valueTypeAdapter.write(memberValue, this, out, false)
        }
      }

      t match {
        case sjc: SJCapture =>
          sjc.captured.foreach {
            case (memberName, capturedValue) =>
              if (first)
                first = false
              else
                out += ','
              writeString(memberName, out)
              out += ':'
              this.jackFlavor.anyTypeAdapter.write(capturedValue, this, out, false)
          }
        case _ =>
      }

      out += '}'
    }
  }

  def writeTuple(writeFns: List[(Transceiver[String], Builder[Any, String]) => Unit], out: Builder[Any, String]): Unit = {
    out += '['
    var first = true
    writeFns.map { f =>
      if (!first)
        out += ','
      else
        first = false
      f(this, out)
    }
    out += ']'
  }
}
