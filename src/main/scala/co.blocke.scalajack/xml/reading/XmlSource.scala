package co.blocke.scalajack
package xml
package reading

import javax.xml.stream.XMLEventReader
import com.ctc.wstx.stax.WstxInputFactory
import java.io.StringReader
import javax.xml.stream.events.{Attribute, Characters, StartElement, XMLEvent}
import scala.jdk.CollectionConverters.*
import shared.StringMatrix

// Wrapper for XMLEventReader to buffer input so we can look ahead 2 events...something the original API can't do.
class BufferedXMLEventReader(delegate: XMLEventReader):
  private val buffer = scala.collection.mutable.Queue.empty[XMLEvent]

  // Peek ahead by index (0 = next event, 1 = second ahead)
  def peek(n: Int = 0): XMLEvent =
    while buffer.size <= n && delegate.hasNext do buffer.enqueue(delegate.nextEvent())
    buffer(n)

  // Regular nextEvent, with buffering awareness
  def nextEvent(): XMLEvent =
    if buffer.nonEmpty then buffer.dequeue()
    else delegate.nextEvent()

  def hasNext: Boolean =
    buffer.nonEmpty || delegate.hasNext

case class XmlSource(rawXML: String):

  private val xmlEventSrc = new BufferedXMLEventReader((new WstxInputFactory()).createXMLEventReader(new StringReader(rawXML)))

  def expectObjectStart(label: String): Option[Map[String, String]] = // (self-closed, attributes)
    if xmlEventSrc.hasNext then
      if xmlEventSrc.peek().isStartDocument then xmlEventSrc.nextEvent() // skip
      val e = xmlEventSrc.nextEvent()
      if !e.isStartElement then None
      else
        val se = e.asStartElement()
        if se.getName.getLocalPart != label then None // wrong object found
        else
          val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
          val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
          Some(attrMap)
    else None

  def nextIsEmpty: Boolean =
    val ahead = xmlEventSrc.peek()
    ahead == null || ahead.asInstanceOf[XMLEvent].isEndElement

  def expectObjectEnd(label: String): Boolean =
    var found = false
    while xmlEventSrc.hasNext && !found do
      val e = xmlEventSrc.nextEvent()
      if e.isEndElement && e.asEndElement.getName.getLocalPart == label then found = true
    found

  def expectObjectField: Option[(String, Map[String, String])] =
    val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
    if ahead == null || !ahead.isStartElement then None
    else
      val se = xmlEventSrc.nextEvent().asStartElement()
      val label = se.getName.getLocalPart
      println(">>> Read field label " + label)
      val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
      val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
      Some((label, attrMap))

  def expectSimpleValue(): Option[String] = // <foo>simple value</foo>
    val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
    if ahead == null || !ahead.isCharacters then None
    else Some(xmlEventSrc.nextEvent().asCharacters().getData)

  def skipValue(): Unit =
    if xmlEventSrc.hasNext then xmlEventSrc.nextEvent() // skip Characters event

  def identifyFieldNum(nameFound: String, fieldNameMatrix: StringMatrix): Int = // returns index of field name or -1 if not found
    var fi: Int = 0
    var bs: Long = fieldNameMatrix.initial
    nameFound.foreach { c =>
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
    }
    bs = fieldNameMatrix.exact(bs, fi)
    fieldNameMatrix.first(bs)

  // For naked arrays (unwrapped) the first start element has already been consumed, and we must specifically
  // leave the last end element unread
  def expectArray[E](entryLabel: String, f: () => E, isNaked: Boolean = false): scala.collection.mutable.ListBuffer[E] =
    val seq = scala.collection.mutable.ListBuffer.empty[E]
    var done = false

    // If !isNaked, test and consume the expected StartElement. Now the 2 cases are on equal footing to start...
    if !isNaked then
      val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
      if ahead == null || !ahead.isStartElement || (ahead.isStartElement && ahead.asStartElement().getName.getLocalPart != entryLabel) then done = true
      else xmlEventSrc.nextEvent()

    while !done do
      seq.append(f())
      if isNaked && !(xmlEventSrc.peek(1).isStartElement && xmlEventSrc.peek(1).asStartElement().getName.getLocalPart == entryLabel) then done = true
      else
        if !expectObjectEnd(entryLabel) then throw new ParseError("Expected entry element for " + entryLabel + " not found")
        val ahead = xmlEventSrc.peek()
        if ahead == null || !ahead.isStartElement || (ahead.isStartElement && ahead.asStartElement().getName.getLocalPart != entryLabel) then done = true
        else xmlEventSrc.nextEvent()
    seq
