package co.blocke.scalajack
package json
package writing

case class JsonOutput():
  val internal: StringBuilder = new StringBuilder()

  private var comma: Boolean = false

  def result = internal.result

  def clear() =
    internal.clear()
    this

  inline def startObject(): Unit =
    maybeComma()
    internal.append('{')
    comma = false

  inline def endObject(): Unit =
    internal.append('}')
    comma = true

  inline def startArray(): Unit =
    maybeComma()
    internal.append('[')
    comma = false

  inline def endArray(): Unit =
    internal.append(']')
    comma = true

  inline def maybeComma(): Unit =
    if comma then internal.append(',')
    comma = false

  inline def burpNull(): Unit =
    internal.append("null")

  inline def label(s: String): Unit =
    maybeComma()
    internal.append("\"" + s + "\":")

  inline def label(s: Long): Unit =
    maybeComma()
    internal.append("\"" + s + "\":")

  // ----------------------- Primitive/Simple type support

  // TODO: BigDecimal, BigInt and Java equiv.

  inline def value(v: Boolean): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Byte): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Char): Unit =
    maybeComma()
    internal.append("\"" + v + "\"")
    comma = true

  inline def value(v: Double): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Float): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Int): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Long): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: Short): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def value(v: String): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append("\"" + v + "\"")
    comma = true

  inline def value(v: java.lang.Boolean): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Byte): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Character): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append("\"" + v + "\"")
    comma = true

  inline def value(v: java.lang.Double): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Float): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Integer): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Long): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Short): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def value(v: java.lang.Number): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  // TODO: UUID
