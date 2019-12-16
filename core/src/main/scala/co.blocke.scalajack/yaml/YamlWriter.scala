package co.blocke.scalajack
package yaml

import model._

import scala.collection.mutable
import ClassHelper.ExtraFieldValue

import scala.collection.Map
import typeadapter.TupleTypeAdapterFactory

case class YamlWriter() extends Writer[String] {

  @inline def addString(s: String, out: mutable.Builder[String, String]): Unit =
    out += s

  private var indent = 0
  private val IDENT = 2
  private val QUEST_IDENT = 2

  private var questIndentTrigger: Boolean = false

  private val LIST_PREFIX = "- "
  private val MAP_PREFIX = ": "
  private val QUEST_PREFIX = "? "
  private var stringWrapIndent = 1

  @inline private def tabIn(): Unit = indent += IDENT

  @inline private def tabOut(): Unit = indent -= IDENT

  @inline private def questIn(): Unit = indent += QUEST_IDENT

  @inline private def questOut(): Unit = indent -= QUEST_IDENT

  @inline private def tab_prefix: String = " " * indent

  def writeArray[Elem](
      t:               Iterable[Elem],
      elemTypeAdapter: TypeAdapter[Elem],
      out:             mutable.Builder[String, String]): Unit = t match {
    case null =>
      writeNull(out)
      out += "\n"
      if (questIndentTrigger) {
        questIn()
        questIndentTrigger = false
      }
    case a =>
      val iter = a.iterator
      while (iter.hasNext) {
        out += tab_prefix + LIST_PREFIX
        elemTypeAdapter match {
          case _: Collectionish =>
            out += "\n"
            tabIn()
            elemTypeAdapter.write(iter.next, this, out)
            tabOut()
          case _ =>
            stringWrapIndent = LIST_PREFIX.length + 1
            elemTypeAdapter.write(iter.next, this, out)
            stringWrapIndent = 1
            out += "\n"
        }
        if (questIndentTrigger) {
          questIn()
          questIndentTrigger = false
        }
      }
  }

  def writeBigInt(t: BigInt, out: mutable.Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeBoolean(t: Boolean, out: mutable.Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeDecimal(t: BigDecimal, out: mutable.Builder[String, String]): Unit =
    t match {
      case null => addString("null", out)
      case s    => addString(s.toString, out)
    }

  def writeDouble(t: Double, out: mutable.Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeInt(t: Int, out: mutable.Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeLong(t: Long, out: mutable.Builder[String, String]): Unit =
    addString(t.toString, out)

  def writeMap[Key, Value, To](
      t:                Map[Key, Value],
      keyTypeAdapter:   TypeAdapter[Key],
      valueTypeAdapter: TypeAdapter[Value],
      out:              mutable.Builder[String, String]): Unit =
    t match {
      case null =>
        writeNull(out)
        out += "\n"
        if (questIndentTrigger) {
          questIn()
          questIndentTrigger = false
        }
      case daMap =>
        daMap.foreach {
          case (key, value) =>
            if (key == null)
              throw new ScalaJackError("Map keys cannot be null.")
            out += tab_prefix
            keyTypeAdapter match {
              case _: Collectionish =>
                out += QUEST_PREFIX
                questIndentTrigger = true
                keyTypeAdapter.write(key, this, out)
                questOut()
                out += tab_prefix
              case _: Stringish if key.toString contains "\n" =>
                out += QUEST_PREFIX
                questIndentTrigger = true
                keyTypeAdapter.write(key, this, out)
                questOut()
                out += "\n" + tab_prefix
              case _ => keyTypeAdapter.write(key, this, out)
            }
            out += MAP_PREFIX
            valueTypeAdapter match {
              case _: Collectionish =>
                out += "\n"
                tabIn()
                valueTypeAdapter.write(value, this, out)
                tabOut()
              case _ =>
                valueTypeAdapter.write(value, this, out)
                out += "\n"
            }
            if (questIndentTrigger) {
              questIn()
              questIndentTrigger = false
            }
        }
    }

  def writeString(t: String, out: mutable.Builder[String, String]): Unit =
    if (t == null) {
      writeNull(out)
      if (questIndentTrigger) {
        questIn()
        questIndentTrigger = false
      }
    } else {
      var stringIndent = 0
      var nlFound = false
      if (t.contains('\n')) {
        out += "|\n"
        stringIndent = stringWrapIndent + indent
        out += " " * stringIndent
        nlFound = true
      }
      t.toCharArray.map {
        case '"'  => addString("""\"""", out)
        case '\\' => addString("""\\""", out)
        case '\b' => addString("""\b""", out)
        case '\f' => addString("""\f""", out)
        case '\r' => addString("""\r""", out)
        case '\t' => addString("""\t""", out)
        case '\n' => addString("\n" + (" " * stringIndent), out)
        case ch if ch < 32 || ch >= 128 =>
          addString("""\""" + "u" + "%04x".format(ch.toInt), out)
        case c => out += c.toString
      }
      if (questIndentTrigger) {
        questIn()
        questIndentTrigger = false
      }
    }

  def writeRaw(t: Any, out: mutable.Builder[String, String]): Unit =
    addString(t.asInstanceOf[String], out)

  def writeNull(out: mutable.Builder[String, String]): Unit =
    addString("null", out)

  @inline private def writeFields(
      isFirst: Boolean,
      fields:  List[(String, Any, TypeAdapter[Any])],
      out:     mutable.Builder[String, String]
  ): Boolean = {
    var first = isFirst
    for ((label, value, valueTypeAdapter) <- fields)
      if (value != None) {
        if (first)
          first = false
        else
          out += ","
        writeString(label, out)
        out += ":"
        valueTypeAdapter.write(value, this, out)
      }
    first
  }

  // TODO
  def writeObject[T](
      t:                  T,
      orderedFieldNames:  List[String],
      fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
      out:                mutable.Builder[String, String],
      extras:             List[(String, ExtraFieldValue[_])]
  ): Unit = {
    if (t == null) {
      addString("null", out)
    } else {
      out += "{"
      val wasFirst = writeFields(
        isFirst = true,
        extras.map(
          e =>
            (
              e._1,
              e._2.value,
              e._2.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]]
            )
        ),
        out
      )
      val wasFirst2 = writeFields(
        wasFirst,
        orderedFieldNames
          .map { fieldName => // Strictly-speaking JSON has no order, but it's clean to write out in constructor order.
            val oneField = fieldMembersByName(fieldName)
            (fieldName, oneField.valueIn(t), oneField.valueTypeAdapter)
          },
        out
      )
      t match {
        case sjc: SJCapture =>
          import scala.jdk.CollectionConverters._
          var first = wasFirst2
          sjc.captured.asScala.foreach {
            case (field, fvalue) =>
              if (first)
                first = false
              else
                out += ","
              writeString(field, out)
              out += ":"
              out += fvalue
                .asInstanceOf[String] // all json captured things are String
          }
        case _ =>
      }
      out += "}"
    }
  }

  def writeTuple[T](
      t:        T,
      writeFns: List[TupleTypeAdapterFactory.TupleField[_]],
      out:      mutable.Builder[String, String]): Unit = {
    writeFns.foreach { f =>
      out += tab_prefix + LIST_PREFIX
      f.write(t, this, out)
      out += "\n"
      if (questIndentTrigger) {
        questIn()
        questIndentTrigger = false
      }
    }
  }
}
