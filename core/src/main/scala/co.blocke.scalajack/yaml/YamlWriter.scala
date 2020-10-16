package co.blocke.scalajack
package yaml

import model._

import scala.collection.mutable
import org.snakeyaml.engine.v2.nodes._
import org.snakeyaml.engine.v2.common.{FlowStyle, ScalarStyle}
import org.snakeyaml.engine.v2.composer.Composer
import org.snakeyaml.engine.v2.events.Event
import org.snakeyaml.engine.v2.resolver.JsonScalarResolver

import scala.jdk.CollectionConverters._

case class YamlWriter() extends Writer[Node] {

  def writeArray[Elem](t: Iterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: mutable.Builder[Node, Node]): Unit = t match {
    case null => writeNull(out)
    case a =>
      val arr    = mutable.ListBuffer.empty[Node]
      val outBuf = YamlBuilder()
      a.iterator.foreach { item =>
        outBuf.clear()
        elemTypeAdapter.write(item, this, outBuf)
        arr += outBuf.result
      }
      val flow = elemTypeAdapter match {
        case _: ScalarTypeAdapter[_] => FlowStyle.FLOW
        case _                       => FlowStyle.BLOCK
      }
      out += new SequenceNode(Tag.SEQ, arr.asJava, flow)
  }

  def writeBigInt(t: BigInt, out: mutable.Builder[Node, Node]): Unit =
    t match {
      case null => writeNull(out)
      case _    => out += new ScalarNode(Tag.INT, t.toString, ScalarStyle.PLAIN)
    }
  def writeBoolean(t: Boolean, out: mutable.Builder[Node, Node]): Unit =
    out += new ScalarNode(Tag.BOOL, t.toString, ScalarStyle.PLAIN)
  def writeDecimal(t: BigDecimal, out: mutable.Builder[Node, Node]): Unit =
    t match {
      case null => writeNull(out)
      case _    => out += new ScalarNode(Tag.FLOAT, t.toString, ScalarStyle.PLAIN)
    }
  def writeDouble(t: Double, out: mutable.Builder[Node, Node]): Unit =
    out += new ScalarNode(Tag.FLOAT, t.toString, ScalarStyle.PLAIN)
  def writeInt(t: Int, out: mutable.Builder[Node, Node]): Unit =
    out += new ScalarNode(Tag.INT, t.toString, ScalarStyle.PLAIN)
  def writeLong(t: Long, out: mutable.Builder[Node, Node]): Unit =
    out += new ScalarNode(Tag.INT, t.toString, ScalarStyle.PLAIN)

  def writeMap[Key, Value, To](t: collection.Map[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: mutable.Builder[Node, Node]): Unit =
    t match {
      case null => writeNull(out)
      case daMap =>
        val outBuf = YamlBuilder()
        val outMap = daMap
          .map {
            case (key, value) =>
              if (key == null)
                throw new ScalaJackError("Map keys cannot be null.")
              outBuf.clear()
              keyTypeAdapter.write(key, this, outBuf)
              val k = outBuf.result()
              outBuf.clear()
              valueTypeAdapter.write(value, this, outBuf)
              new NodeTuple(k, outBuf.result())
          }
          .toList
          .asJava
        out += new MappingNode(Tag.MAP, outMap, FlowStyle.AUTO)
    }

  def writeNull(out: mutable.Builder[Node, Node]): Unit =
    out += new ScalarNode(Tag.NULL, "null", ScalarStyle.PLAIN)

  @inline private def writeFields(
    fields: List[(String, Any, TypeAdapter[Any])]
  ): Map[Node, Node] = {
    val outBuf = YamlBuilder()
    fields.collect {
      case (label, value, valueTypeAdapter) if value != None =>
        outBuf.clear()
        valueTypeAdapter.write(value, this, outBuf)
        new ScalarNode(Tag.STR, label, ScalarStyle.PLAIN) -> outBuf.result()
    }.toMap
  }

  def writeObject[T](
    t: T,
    orderedFieldNames: List[String],
    fieldMembersByName: collection.Map[String, ClassFieldMember[_,_]],
    out: mutable.Builder[Node, Node],
    extras: List[(String, ExtraFieldValue[_])]
  ): Unit = {
    t match {
      case null => writeNull(out.asInstanceOf[collection.mutable.Builder[Node,Node]])
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
          (orn, oneField.info.valueOf(t), oneField.valueTypeAdapter.asInstanceOf[TypeAdapter[Any]])
        })
        val captureFields = t match {
          case sjc: SJCapture =>
            import scala.jdk.CollectionConverters._

            sjc.captured.asScala.map {
              case (k, v) =>
                val composer = new Composer(new EventParser(v.asInstanceOf[List[Event]]), new JsonScalarResolver())
                (new ScalarNode(Tag.STR, k, ScalarStyle.PLAIN), composer.next)
            }
          case _ => Map.empty[Node, Node]
        }

        val mapNodes = (extraFields ++ classFields ++ captureFields).map { case (k, v) => new NodeTuple(k, v) }
        out += new MappingNode(Tag.MAP, mapNodes.toList.asJava, FlowStyle.AUTO).asInstanceOf[Node]
    }
  }

  def writeString(t: String, out: mutable.Builder[Node, Node]): Unit = t match {
    case null => writeNull(out)
    case _    => out += new ScalarNode(Tag.STR, t, ScalarStyle.PLAIN)
  }

  def writeRaw(t: Node, out: mutable.Builder[Node, Node]): Unit =
    out += t

  def writeTuple[T](
      t: T,
      writeFn: (Product) => List[(TypeAdapter[_], Any)],
      out: mutable.Builder[Node, Node]
  ): Unit = {
    val arr    = mutable.ListBuffer.empty[Node]
    val outBuf = YamlBuilder()
    val flowStyle = writeFn(t.asInstanceOf[Product]).foldLeft(true) { case (acc, (fieldTA, fieldValue)) =>
      outBuf.clear()
      fieldTA.castAndWrite(fieldValue, this, outBuf)
      arr += outBuf.result
      if fieldTA.isInstanceOf[ScalarTypeAdapter[_]] then
        acc
      else
        false
    }
    out += new SequenceNode(Tag.SEQ, arr.asJava, {if flowStyle then FlowStyle.FLOW else FlowStyle.BLOCK})
  }
}

// $COVERAGE-OFF$This is a snakeyaml thing.  It works if consuming it works.
case class EventParser(events: List[Event]) extends org.snakeyaml.engine.v2.parser.Parser {
  private var i = 0

  def checkEvent(choice: Event.ID): Boolean =
    if (i < events.length)
      events(i).getEventId == choice
    else
      false
  def peekEvent(): Event = events(i)
  def next(): Event = {
    val ret = events(i)
    i += 1
    ret
  }
  def hasNext: Boolean = i < events.length
  // $COVERAGE-ON$
}