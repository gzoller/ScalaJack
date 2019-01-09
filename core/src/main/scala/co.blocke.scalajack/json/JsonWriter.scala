package co.blocke.scalajack
package json

import model._

import scala.collection.{ GenMap, GenIterable }
import org.apache.commons.text.StringEscapeUtils.escapeJava
import scala.collection.mutable.Builder

trait JsonWriter extends Writer {

  this: JsonTransciever =>

  val stringTypeAdapter: TypeAdapter[String]

  @inline def addString(s: String, out: Builder[Any, String]): Unit = s.toCharArray.map(c => out += c)

  def writeArray[Elem](t: GenIterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += '['
      val iter = a.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this)(out)
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

  def writeMap[Key, Value, To](t: GenMap[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    // Optimization if Key is String.  Don't need to build a sub-StringBuilder to stringify a non-String key
    case a if keyTypeAdapter == stringTypeAdapter =>
      out += '{'
      val iter = a.iterator
      while (iter.hasNext) {
        val kv = iter.next
        writeString(kv._1.asInstanceOf[String], out)
        out += ':'
        valueTypeAdapter.write(kv._2, this)(out)
        if (iter.hasNext)
          out += ','
      }
      out += '}'
    case a =>
      out += '{'
      val iter = a.iterator
      while (iter.hasNext) {
        val kv = iter.next
        val out2 = new StringBuilder().asInstanceOf[Builder[Any, String]] // stringify a non-string key
        keyTypeAdapter.write(kv._1, this)(out2)
        writeString(out2.result(), out)
        out += ':'
        valueTypeAdapter.write(kv._2, this)(out)
        if (iter.hasNext)
          out += ','
      }
      out += '}'
  }

  def writeString(t: String, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case s: String =>
      out += '"'
      addString(escapeJava(s), out)
      out += '"'
  }

}
