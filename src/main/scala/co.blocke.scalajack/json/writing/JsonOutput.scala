package co.blocke.scalajack
package json
package writing

import java.time.format.DateTimeFormatter.*

/** Wrapper around a (Fast)StringBuilder that offers support for primitive types,
  * including quotes-wrapping those that need it for use in Map keys (aka stringified).
  * Handles dangling commas/separators, plus mark & revert capability.  It is reusable
  * via clear() for speed -- one per thread, of course!
  */
case class JsonOutput():
  val internal: FastStringBuilder = new FastStringBuilder()

  private var comma: Boolean = false
  private var savePoint: Int = 0
  private var saveComma: Boolean = false

  def result = internal.result

  def clear() =
    internal.clear()
    comma = false
    savePoint = 0
    saveComma = false
    this

  def mark() =
    savePoint = internal.length
    saveComma = comma

  def revert() = // delete everything after the set savePoint
    internal.setLength(savePoint)
    comma = saveComma

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

  inline def colon(): Unit =
    internal.append(':')
    comma = false

  inline def maybeComma(): Unit =
    if comma then internal.append(',')
    comma = false

  inline def burpNull(): Unit =
    maybeComma()
    internal.append("null")
    comma = true

    // Problem:  for unions, if left fails to write, comma is not reset to true for the attempt to write right side
  inline def label(s: String): Unit =
    maybeComma()
    internal.append('"')
    internal.append(s)
    internal.append('"')
    internal.append(':')

  // ----------------------- Primitive/Simple type support

  inline def value(v: scala.math.BigDecimal): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  // Note: data types that are not naturally quotes-wrapped have "Stringified" variants
  // (saparate vs a param for speed), for use in Map/Json object keys.
  inline def valueStringified(v: scala.math.BigDecimal): Unit =
    maybeComma()
    if v == null then throw new JsonNullKeyValue("Key values may not be null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: scala.math.BigInt): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v.toString)
    comma = true

  inline def valueStringified(v: scala.math.BigInt): Unit =
    maybeComma()
    if v == null then throw new JsonNullKeyValue("Key values may not be null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.math.BigDecimal): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v.toString)
    comma = true

  inline def valueStringified(v: java.math.BigDecimal): Unit =
    maybeComma()
    if v == null then throw new JsonNullKeyValue("Key values may not be null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.math.BigInteger): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v.toString)
    comma = true

  inline def valueStringified(v: java.math.BigInteger): Unit =
    maybeComma()
    if v == null then throw new JsonNullKeyValue("Key values may not be null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: Boolean): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueStringified(v: Boolean): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v.toString)
    internal.append('"')
    comma = true

  inline def value(v: Byte): Unit =
    maybeComma()
    internal.append(v.toInt)
    comma = true

  inline def valueStringified(v: Byte): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v.toInt)
    internal.append('"')
    comma = true

  inline def value(v: Char): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: Double): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueStringified(v: Double): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: Float): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueStringified(v: Float): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: Int): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueSringified(v: Int): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: Long): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueStringified(v: Long): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: Short): Unit =
    maybeComma()
    internal.append(v)
    comma = true

  inline def valueStringified(v: Short): Unit =
    maybeComma()
    internal.append('"')
    internal.append(v)
    internal.append('"')
    comma = true

  inline def value(v: String): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def valueEscaped(v: String): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.appendEscaped(v, 0, v.length)
      internal.append('"')
    comma = true

  inline def valueRaw(v: RawJson): Unit =
    maybeComma()
    internal.append(v.asInstanceOf[String])
    comma = true

  inline def value(v: java.lang.Boolean): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Boolean): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Byte): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v.toInt)
    comma = true

  inline def valueStringified(v: java.lang.Byte): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toInt)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Character): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Double): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Double): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Float): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Float): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Integer): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Integer): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Long): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Long): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Short): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Short): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.lang.Number): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else internal.append(v)
    comma = true

  inline def valueStringified(v: java.lang.Number): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v)
      internal.append('"')
    comma = true

  inline def value(v: java.time.Duration): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.Instant): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.LocalDate): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_LOCAL_DATE))
      internal.append('"')
    comma = true

  inline def value(v: java.time.LocalDateTime): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_LOCAL_DATE_TIME))
      internal.append('"')
    comma = true

  inline def value(v: java.time.LocalTime): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_LOCAL_TIME))
      internal.append('"')
    comma = true

  inline def value(v: java.time.MonthDay): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.OffsetDateTime): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_OFFSET_DATE_TIME))
      internal.append('"')
    comma = true

  inline def value(v: java.time.OffsetTime): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_OFFSET_TIME))
      internal.append('"')
    comma = true

  inline def value(v: java.time.Period): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.Year): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.YearMonth): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.ZonedDateTime): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.format(ISO_ZONED_DATE_TIME))
      internal.append('"')
    comma = true

  inline def value(v: java.time.ZoneId): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.time.ZoneOffset): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.net.URL): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.net.URI): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true

  inline def value(v: java.util.UUID): Unit =
    maybeComma()
    if v == null then internal.append("null")
    else
      internal.append('"')
      internal.append(v.toString)
      internal.append('"')
    comma = true
