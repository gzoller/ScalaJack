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
//    xmlEventSrc.peek() match {
//      case p if p.isStartElement  => println("HERE (SE): " + p.asStartElement.getName.getLocalPart + " for label " + label)
//      case p if p.isStartDocument => println("HERE (SD)")
//      case p                      => println("HERE (something else): " + p.getClass.getName)
//    }
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

  def peekObjectStart: String =
    skipWS()
    if xmlEventSrc.peek().isStartDocument then xmlEventSrc.nextEvent() // skip
    if xmlEventSrc.hasNext then
      val e = xmlEventSrc.peek()
      if !e.isStartElement then throw new ParseError(s"Expected some start element but the next element wasn't a start element.")
      else e.asStartElement.getName.getLocalPart
    else throw new ParseError(s"Expected some start element but there were no more elements to be read.")

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
    val n = xmlEventSrc.peek()
    if n.isCharacters && n.asCharacters.isWhiteSpace then xmlEventSrc.nextEvent()

  def expectObjectField: Option[(String, Map[String, String])] =
    skipWS()
    val ahead = xmlEventSrc.peek()
    if ahead == null || !ahead.isStartElement then None
    else
      val se = xmlEventSrc.nextEvent().asStartElement()
      val label = se.getName.getLocalPart
//      println(">>> Read field label " + label)
      val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
      val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
//      println("Object Field Found: " + label)
      Some((label, attrMap))

  def expectSimpleValue(): Option[String] = // <foo>simple value</foo>
    val ahead = xmlEventSrc.peek()
    if ahead == null || !ahead.isCharacters then None
    else Some(xmlEventSrc.nextEvent().asCharacters().getData)

  def skipValue(endLabel: String): Unit =
    while xmlEventSrc.hasNext && !isEndMatch(xmlEventSrc.peek(), endLabel) do xmlEventSrc.nextEvent() // skip Characters event
    if xmlEventSrc.hasNext then xmlEventSrc.nextEvent() // skip end element

  def identifyFieldNum(nameFound: String, fieldNameMatrix: StringMatrix): Int = // returns index of field name or -1 if not found
    var fi: Int = 0
    var bs: Long = fieldNameMatrix.initial
    nameFound.foreach { c =>
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
    }
    bs = fieldNameMatrix.exact(bs, fi)
    fieldNameMatrix.first(bs)

  private inline def isStartMatch(e: XMLEvent, tags: List[String]): Option[String] =
    if e != null && e.isStartElement then tags.find(_ == e.asStartElement().getName.getLocalPart)
    else None
  private inline def isEndMatch(e: XMLEvent, tag: String): Boolean =
    e != null && e.isEndElement && e.asEndElement().getName.getLocalPart == tag
  private inline def isEndMatch(e: XMLEvent): Option[String] =
    if e != null && e.isEndElement then Some(e.asEndElement().getName.getLocalPart)
    else None

  // For naked arrays (unwrapped) the first start element has already been consumed, and we must specifically
  // leave the last end element unread.
  // We have 3 cases:
  // 1. Entries wrapped with entryLabel tags + NORMAL -> consume entryLabel and parse object with f()
  // 2. Entries wrapped with entryLabel tags + NAKED -> don't consume entryLabel: parse object with f() (entryLabel part of the object)
  // 3. Entries wrapped with entryLabel tags + STRUCT -> presume entry label already consumed
  def expectArray[E](entryLabels: List[String], f: () => E, mode: InputMode): scala.collection.mutable.ListBuffer[E] =
    val seq = scala.collection.mutable.ListBuffer.empty[E]
    var done = false

//    println(">>> Expecting array with label " + entryLabels + " naked? " + mode)

    while !done do
      skipWS()
      val endLabel = mode match {
        case InputMode.NORMAL => // consume start element for entryLabel
          isStartMatch(xmlEventSrc.peek(), entryLabels) match {
            case Some(e) =>
              xmlEventSrc.nextEvent() // skip entry wrapper
              e
            case None =>
              done = true
              ""
          }
        case InputMode.NAKED => // don't consume entryLabel element
          isStartMatch(xmlEventSrc.peek(), entryLabels) match {
            case Some(e) =>
              e
            case None =>
              done = true
              ""
          }
        case _ =>
          "" // alreadh consumed entryLabel before calling expectArray for Struct mode
      }

      if !done then
        // Read list item
        skipWS()
//        println("begin parsing thingy: " + xmlEventSrc.peek().asStartElement().getName.getLocalPart)
        seq.append(f())
//        println("MID: " + seq)
        skipWS()

        // Process end element
//        println("Array element read over... next: " + xmlEventSrc.peek().getClass.getName)
        mode match {
          case InputMode.NORMAL => // consume end element for entryLabel
            if isEndMatch(xmlEventSrc.peek(), endLabel) then xmlEventSrc.nextEvent()
            else throw new ParseError("Expeced required end element for " + endLabel + " not found")
          case InputMode.STRUCT => // consume end element for entryLabel
//            println("---$$$ " + showElement(xmlEventSrc.peek()))
            isEndMatch(xmlEventSrc.peek()) match {
              case Some(e) if entryLabels.contains(e) =>
//                println("Found Struct end: " + e)
                skipWS()
                //              println("---$$$2 " + showElement(xmlEventSrc.peek(1)))
                val lookAhead = peekNextAfterWS(1)
                if isStartMatch(lookAhead, entryLabels).isDefined then
                  xmlEventSrc.nextEvent() // consume end element
                  skipWS()
                  xmlEventSrc.nextEvent()
                //                println("---$$$3 " + showElement(xmlEventSrc.peek()))
                else done = true
              case _ => throw new ParseError("Expeced required end element for " + endLabel + " not found")
            }
          case _ => // end already consumed for other cases
        }

//    println("FINSIHED: " + seq)
//    println("Next up: " + showElement(xmlEventSrc.peek()))
    seq

  def isNull: Boolean =
    val p = xmlEventSrc.peek()
    if p.isCharacters && p.asCharacters.getData == "null" then
      xmlEventSrc.nextEvent() // skip null
      true
    else false

  private def peekNextAfterWS(i: Int): XMLEvent =
    xmlEventSrc.peek(i) match {
      case e if e.isCharacters && e.asCharacters.isWhiteSpace => xmlEventSrc.peek(i + 1)
      case e                                                  => e
    }

  private def showElement(e: XMLEvent): String =
    e match {
      case y if e.isStartElement  => "Start::" + e.asStartElement.getName.getLocalPart
      case y if e.isEndElement    => "End::" + e.asEndElement.getName.getLocalPart
      case y if e.isCharacters    => "Characters::" + e.asCharacters.getData
      case y if e.isStartDocument => "Start Document"
      case _                      => "unknown"
    }
