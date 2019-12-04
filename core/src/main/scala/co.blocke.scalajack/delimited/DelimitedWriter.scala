package co.blocke.scalajack
package delimited

import model._
import ClassHelper.ExtraFieldValue

import scala.collection.Map
import scala.collection.mutable

case class DelimitedWriter(delimiter: Char) extends Writer[String] {

  def writeArray[Elem](
      t:               Iterable[Elem],
      elemTypeAdapter: TypeAdapter[Elem],
      out:             mutable.Builder[String, String]): Unit =
    if (t != null) {
      val sb = compat.StringBuilder()
      val iter = t.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this, sb)
        if (iter.hasNext)
          sb += delimiter.toString
      }
      writeString(sb.result(), out)
    }

  def writeBigInt(t: BigInt, out: mutable.Builder[String, String]): Unit =
    out += t.toString
  def writeBoolean(t: Boolean, out: mutable.Builder[String, String]): Unit =
    out += t.toString
  def writeDecimal(t: BigDecimal, out: mutable.Builder[String, String]): Unit =
    out += t.toString
  def writeDouble(t: Double, out: mutable.Builder[String, String]): Unit =
    out += t.toString
  def writeInt(t: Int, out: mutable.Builder[String, String]): Unit =
    out += t.toString
  def writeLong(t: Long, out: mutable.Builder[String, String]): Unit =
    out += t.toString

  def writeMap[Key, Value, To](
      t:                Map[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              mutable.Builder[String, String]): Unit =
    throw new ScalaJackError(
      "Map-typed data is not supported for delimited output"
    )

  def writeNull(out: mutable.Builder[String, String]): Unit = {} // write empty field

  def writeObject[T](
      t:                  T,
      orderedFieldNames:  List[String],
      fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
      out:                mutable.Builder[String, String],
      extras:             List[(String, ExtraFieldValue[_])]                = List.empty[(String, ExtraFieldValue[_])]
  ): Unit =
    if (t != null) {
      var first = true
      orderedFieldNames.foreach { name =>
        if (first)
          first = false
        else
          out += delimiter.toString
        val f = fieldMembersByName(name)
        f.valueTypeAdapter match {
          case ta if ta.isInstanceOf[Classish] =>
            val sb = compat.StringBuilder()
            ta.write(f.valueIn(t), this, sb)
            writeString(sb.result(), out)
          case ta =>
            ta.write(f.valueIn(t), this, out)
        }
      }
    }

  def writeString(t: String, out: mutable.Builder[String, String]): Unit =
    if (t != null) {
      val t0 = t.replaceAll("\"", "\"\"")
      val toWrite =
        if (t0 != t || t0.contains(delimiter))
          "\"" + t0 + "\""
        else
          t
      out += toWrite
    }
  // $COVERAGE-OFF$Never called for delimited output
  def writeRaw(t: Any, out: mutable.Builder[String, String]): Unit =
    writeString(t.asInstanceOf[String], out)
  // $COVERAGE-ON$

  def writeTuple[T](
      t:        T,
      writeFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]],
      out:      mutable.Builder[String, String]
  ): Unit = {
    var first = true
    val sb = compat.StringBuilder()
    writeFns.foreach { f =>
      if (first)
        first = false
      else
        sb += delimiter.toString
      f.valueTypeAdapter match {
        case cta: Classish =>
          val sb2 = compat.StringBuilder()
          f.write(t, this, sb2)
          writeString(sb2.result(), sb)
        case ta => f.write(t, this, sb)
      }
    }
    writeString(sb.result(), out)
  }
}
