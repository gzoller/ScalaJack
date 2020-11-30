package co.blocke.scalajack
package delimited

import model._
import co.blocke.scalajack.model

import scala.collection.Map
import scala.collection.mutable

case class DelimitedWriter(delimiter: Char) extends Writer[DELIMITED] {

  def writeArray[Elem](t: Iterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    if (t != null) {
      val sb   = model.StringBuilder[DELIMITED]()
      val iter = t.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this, sb)
        if (iter.hasNext)
          sb += delimiter.toString.asInstanceOf[DELIMITED]
      }
      writeString(sb.result().asInstanceOf[String], out)
    }

  def writeBigInt(t: BigInt, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]
  def writeBoolean(t: Boolean, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]
  def writeDecimal(t: BigDecimal, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]
  def writeDouble(t: Double, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]
  def writeInt(t: Int, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]
  def writeLong(t: Long, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    out += t.toString.asInstanceOf[DELIMITED]

  def writeMap[Key, Value, To](t: Map[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    throw new ScalaJackError(
      "Map-typed data is not supported for delimited output"
    )

  def writeNull(out: mutable.Builder[DELIMITED, DELIMITED]): Unit = {} // write empty field

  def writeObject[T](
      t: T,
      orderedFieldNames: List[String],
      fieldMembersByName: Map[String, ClassFieldMember[_,_]],
      out: mutable.Builder[DELIMITED, DELIMITED],
      extras: List[(String, ExtraFieldValue[_])] = List.empty[(String, ExtraFieldValue[_])]
  ): Unit =
    if (t != null) {
      var first = true
      orderedFieldNames.foreach { name =>
        if (first)
          first = false
        else
          out += delimiter.toString.asInstanceOf[DELIMITED]
        val f = fieldMembersByName(name)
        f.valueTypeAdapter match {
          case ta if ta.isInstanceOf[Classish] =>
            val sb = model.StringBuilder[DELIMITED]()
            ta.castAndWrite(f.info.valueOf(t), this, sb)
            writeString(sb.result().asInstanceOf[String], out)
          case ta =>
            ta.castAndWrite(f.info.valueOf(t), this, out)
        }
      }
    }

  def writeString(t: String, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    if (t != null) {
      val t0 = t.replaceAll("\"", "\"\"")
      val toWrite =
        if (t0 != t || t0.contains(delimiter))
          "\"" + t0 + "\""
        else
          t
      out += toWrite.asInstanceOf[DELIMITED]
    }

  // $COVERAGE-OFF$Never called for delimited output
  def writeRaw(t: DELIMITED, out: mutable.Builder[DELIMITED, DELIMITED]): Unit =
    writeString(t.asInstanceOf[String], out)
  // $COVERAGE-ON$

  def writeTuple[T](
      t: T,
      writeFn: (Product) => List[(TypeAdapter[_], Any)],
      out: mutable.Builder[DELIMITED, DELIMITED]
  ): Unit = {
    var first = true
    val sb    = model.StringBuilder[DELIMITED]()
    writeFn(t.asInstanceOf[Product]).foreach { (fieldTA, fieldValue) =>
      if (first)
        first = false
      else
        sb += delimiter.toString.asInstanceOf[DELIMITED]
      fieldTA match {
        case cta: Classish =>
          val sb2 = model.StringBuilder[DELIMITED]()
          fieldTA.castAndWrite(fieldValue, this, sb2)
          writeString(sb2.result().asInstanceOf[String], sb)
        case ta => 
          fieldTA.castAndWrite(fieldValue, this, sb)
      }
    }
    writeString(sb.result().asInstanceOf[String], out)
  }
}
