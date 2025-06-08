package co.blocke.scalajack
package xml
package reading

import com.ctc.wstx.stax.WstxInputFactory
import java.io.StringReader
import javax.xml.stream.events.{Attribute, Characters, StartElement, XMLEvent}
import scala.jdk.CollectionConverters.*
import shared.StringMatrix

case class XmlSource(rawXML: String):

  private val xmlEventSrc = (new WstxInputFactory()).createXMLEventReader(new StringReader(rawXML))

  def expectObjectStart(label: String): Option[Map[String, String]] = // (self-closed, attributes)
    if xmlEventSrc.hasNext() then
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
    ahead == null || ahead.asInstanceOf[XMLEvent].isEndElement()

  def expectObjectEnd(label: String): Boolean =
    var found = false
    while xmlEventSrc.hasNext() && !found do
      val e = xmlEventSrc.nextEvent()
      if e.isEndElement && e.asEndElement.getName.getLocalPart == label then found = true
    found

  def expectObjectField: Option[(String, Map[String, String])] =
    val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
    if ahead == null || !ahead.isStartElement then None
    else
      val se = xmlEventSrc.nextEvent().asStartElement()
      val label = se.getName.getLocalPart
      val attrs = se.getAttributes.asScala.map(_.asInstanceOf[Attribute]).toList
      val attrMap = attrs.map(a => a.getName.getLocalPart -> a.getValue).toMap
      Some((label, attrMap))

  def expectSimpleValue(): Option[String] = // <foo>simple value</foo>
    val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
    if ahead == null || !ahead.isCharacters then None
    else Some(xmlEventSrc.nextEvent().asCharacters().getData)

//  def expectEntryValue(entryLabel: String): Boolean =
//    val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
//    if ahead == null || !ahead.isStartElement then
//      false
//    else
//      val se = xmlEventSrc.nextEvent().asStartElement()
//      se.getName.getLocalPart == entryLabel

  def skipValue(): Unit =
    if xmlEventSrc.hasNext() then xmlEventSrc.nextEvent() // skip Characters event

  def identifyFieldNum(nameFound: String, fieldNameMatrix: StringMatrix): Int = // returns index of field name or -1 if not found
    var fi: Int = 0
    var bs: Long = fieldNameMatrix.initial
    nameFound.foreach { c =>
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
    }
    bs = fieldNameMatrix.exact(bs, fi)
    fieldNameMatrix.first(bs)

  def expectArray[E](entryLabel: String, f: () => E): scala.collection.mutable.ListBuffer[E] =
    val seq = scala.collection.mutable.ListBuffer.empty[E]
    var done = false
    while !done do
      val ahead = xmlEventSrc.peek().asInstanceOf[XMLEvent]
      if ahead == null || !ahead.isStartElement then done = true
      else
        val se = xmlEventSrc.nextEvent().asStartElement()
        if se.getName.getLocalPart != entryLabel then throw new ParseError("Expected entry element for " + entryLabel + " not found")
        seq.append(f())
        expectObjectEnd(entryLabel)
        ()
    seq
