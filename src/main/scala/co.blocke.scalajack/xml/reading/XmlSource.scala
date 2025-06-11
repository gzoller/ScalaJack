package co.blocke.scalajack
package xml
package reading

import javax.xml.stream.XMLEventReader
import com.ctc.wstx.stax.WstxInputFactory
import java.io.StringReader
import javax.xml.stream.events.{Attribute, Characters, StartElement, XMLEvent}
import scala.jdk.CollectionConverters.*
import shared.StringMatrix

enum InputMode:
  case NORMAL, NAKED, STRUCT

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
//  while xmlEventSrc.hasNext do {
//    val evt = xmlEventSrc.nextEvent()
//    println(s"${evt.getEventType} → $evt")
//  }

  def expectObjectStart(label: String): Map[String, String] = // (self-closed, attributes)
    skipWS()
    xmlEventSrc.peek() match {
      case p if p.isStartElement  => println("HERE (SE): " + p.asStartElement.getName.getLocalPart + " for label " + label)
      case p if p.isStartDocument => println("HERE (SD)")
      case p                      => println("HERE (something else): " + p.getClass.getName)
    }
    if xmlEventSrc.hasNext then
      if xmlEventSrc.peek().isStartDocument then xmlEventSrc.nextEvent() // skip
      val e = xmlEventSrc.nextEvent()
      if !e.isStartElement then throw new ParseError(s"Expected start element $label but the next element wasn't a start element.")
      else
        val se = e.asStartElement()
        if se.getName.getLocalPart != label then throw new ParseError(s"Expected start element $label but start element ${se.getName.getLocalPart} was found instead.")
        else
          val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
          val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
          attrMap
    else throw new ParseError(s"Expected start element $label but there were no more elements to be read.")

  def nextIsEmpty: Boolean =
    val ahead = xmlEventSrc.peek()
    ahead == null || ahead.isEndElement

  def expectObjectEnd(label: String): Boolean =
    var found = false
    while xmlEventSrc.hasNext && !found do
      val e = xmlEventSrc.nextEvent()
      if e.isEndElement && (e.asEndElement.getName.getLocalPart == label) then found = true
    found

  inline def skipWS() =
    var skippedWS = false
    while !skippedWS do
      xmlEventSrc.peek() match {
        case p if p.isCharacters && p.asCharacters.isWhiteSpace =>
          xmlEventSrc.nextEvent()
        case p if p.isStartElement =>
          skippedWS = true
        case p =>
          skippedWS = true
      }

  def expectObjectField: Option[(String, Map[String, String])] =
    skipWS()
    val ahead = xmlEventSrc.peek()
    if ahead == null || !ahead.isStartElement then None
    else
      val se = xmlEventSrc.nextEvent().asStartElement()
      val label = se.getName.getLocalPart
      println(">>> Read field label " + label)
      val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
      val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
      Some((label, attrMap))

  def expectSimpleValue(): Option[String] = // <foo>simple value</foo>
    val ahead = xmlEventSrc.peek()
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

  private inline def isStartMatch(e: XMLEvent, tag: String): Boolean =
    e != null && e.isStartElement && e.asStartElement().getName.getLocalPart == tag
  private inline def isEndMatch(e: XMLEvent, tag: String): Boolean =
    e != null && e.isEndElement && e.asEndElement().getName.getLocalPart == tag

  // For naked arrays (unwrapped) the first start element has already been consumed, and we must specifically
  // leave the last end element unread.
  // We have 3 cases:
  // 1. Entries wrapped with entryLabel tags + NORMAL -> consume entryLabel and parse object with f()
  // 2. Entries wrapped with entryLabel tags + NAKED -> don't consume entryLabel: parse object with f() (entryLabel part of the object)
  // 3. Entries wrapped with entryLabel tags + STRUCT -> presume entry label already consumed
  def expectArray[E](entryLabel: String, f: () => E, mode: InputMode): scala.collection.mutable.ListBuffer[E] =
    val seq = scala.collection.mutable.ListBuffer.empty[E]
    var done = false

    println(">>> Expecting array with label " + entryLabel + " naked? " + mode)

    while !done do
      mode match {
        case InputMode.NORMAL => // consume start element for entryLabel
          if isStartMatch(xmlEventSrc.peek(), entryLabel) then xmlEventSrc.nextEvent()
          else done = true
        case InputMode.NAKED => // don't consume entryLabel element
          if !isStartMatch(xmlEventSrc.peek(), entryLabel) then done = true
        case _ =>
      }

      if !done then
        // Read list item
        skipWS()
        println("begin parsing thingy: " + xmlEventSrc.peek().asStartElement().getName.getLocalPart)
        seq.append(f())
        println("MID: " + seq)
        skipWS()

        // Process end element
        println("Array element read over... next: " + xmlEventSrc.peek().getClass.getName)
        mode match {
          case InputMode.NORMAL => // consume end element for entryLabel
            if isEndMatch(xmlEventSrc.peek(), entryLabel) then xmlEventSrc.nextEvent()
            else throw new ParseError("Expeced required end element for " + entryLabel + " not found")
          case InputMode.STRUCT => // consume end element for entryLabel
            if isEndMatch(xmlEventSrc.peek(), entryLabel) then
              if isStartMatch(xmlEventSrc.peek(1), entryLabel) then
                xmlEventSrc.nextEvent() // consume end element
                xmlEventSrc.nextEvent() // pre-consume start element
              else done = true
            else throw new ParseError("Expeced required end element for " + entryLabel + " not found")
          case _ => // end already consumed for other cases
        }

    println("FINSIHED: " + seq)
    seq

/*
    <foo><bar>...</bar></foo>
    <foo><bar>...</bar></foo>
    <foo><bar>...</bar></foo>
    <blah/>
 */
