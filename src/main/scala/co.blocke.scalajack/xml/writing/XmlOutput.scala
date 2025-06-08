package co.blocke.scalajack
package xml
package writing

import co.blocke.scalajack.shared.FastStringBuilder

class XmlOutput():
  val internal: FastStringBuilder = new FastStringBuilder()

  private var justClosed: Boolean = false
  private var savePoint: Int = 0

  def result: String = internal.result

  def clear(): XmlOutput =
    internal.clear()
    justClosed = false
    savePoint = 0
    this

  def mark(): Unit =
    savePoint = internal.length

  def revert(): Unit = // delete everything after the set savePoint
    internal.setLength(savePoint)

  inline def startElement(label: String): Unit =
    internal.append(s"<$label>")
    justClosed = false

  inline def endElement(label: String): Unit =
    internal.append(s"</$label>")
    justClosed = true

  inline def emptyElement(label: String): Unit =
    internal.append(s"<$label/>")
    justClosed = true

  inline def emitValue(v: String): Unit =
    if v == "" && internal.peekBack().contains('>') then
      internal.backspace()
      internal.append("/>")
      justClosed = true
    else internal.append(v)

  // For elements w/attributes
  inline def openElement(label: String): Unit =
    internal.append(s"<$label")
    justClosed = false

  inline def attribute(label: String): Unit =
    internal.append(s""" $label="""")
    justClosed = false

  inline def closeAttribute(): Unit =
    internal.append('"')

  inline def closeElement(): Unit =
    if !justClosed then internal.append(">")
    justClosed = false

  inline def closeElementEmpty(): Unit =
    if !justClosed then internal.append("/>")

  // Not idiomatic XML to output "null", but too much surgery to fix. Nulls should be anomalous in Scala anyway!
  inline def burpNull(): Unit =
    internal.append("null")
    justClosed = false
