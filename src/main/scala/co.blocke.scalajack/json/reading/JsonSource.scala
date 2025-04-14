package co.blocke.scalajack
package json
package reading

import scala.annotation.{switch, tailrec}
import co.blocke.scalajack.internal.SafeNumbers.double
import co.blocke.scalajack.internal.UnsafeNumbers

object JsonSource:
  val ull: Array[Char] = "ull".toCharArray
  protected val alse: Array[Char] = "alse".toCharArray
  protected val rue: Array[Char] = "rue".toCharArray
  protected val falseBytes = 'f' | 'a' << 8 | 'l' << 16 | 's' << 24 | 'e' << 32
  protected val trueBytes = 't' | 'r' << 8 | 'u' << 16 | 'e' << 24

// ZIO-Json defines a series of different Readers.  Not exactly sure why--maybe to support different
// modes (streaming, ...)? At least for now we only need one, so merged key bits of Readers into one.
case class JsonSource(js: CharSequence):

  private var i = 0
  val max = js.length

  // Navigation...
  // =======================================================

  def pos = i

  inline def here = js.charAt(i)

  inline def backspace() = i -= 1

  inline def revertToPos(p: Int) = i = p
  inline def captureMark(mark: Int): String = js.subSequence(mark, i).toString

  @tailrec
  final def readToken(): Char =
    if i == max then throw new JsonParseError("Unexpected end of buffer", this)
    else
      val b = here
      i += 1
      if !(b == ' ' || b == '\n' || b == '\t' || (b | 0x4) == '\r') then b
      else readToken()

  @tailrec
  final def expectToken(t: Char): Unit =
    if i == max then throw new JsonParseError("Unexpected end of buffer", this)
    else
      val b = here
      i += 1
      if !(b == ' ' || b == '\n' || b == '\t' || (b | 0x4) == '\r') then
        if b != t then
          backspace()
          throw JsonParseError(s"Expected '$t' here", this)
        else ()
      else expectToken(t)

  def expectNull(): Boolean =
    if readToken() == 'n' then
      readChars(JsonSource.ull, "null")
      true
    else
      backspace()
      false

  // Enum...
  // =======================================================
  def expectEnum(): Int | String =
    readToken() match
      case t if t >= '0' && t <= '9' =>
        backspace()
        expectInt()
      case t if t == '"' =>
        val endI = parseString(i)
        val str = js.subSequence(i, endI).toString
        i = endI + 1
        str
      case t if t == 'n' =>
        readChars(JsonSource.ull, "null")
        null.asInstanceOf[String]
      case _ =>
        throw JsonParseError("Expected valid enumeration value or null here", this)

  // Object...
  // =======================================================

  // returns false if 'null' found
  def expectFirstObjectField(fieldNameMatrix: StringMatrix): Option[Int] =
    val t = readToken()
    if t == '{' then
      val tt = readToken()
      if tt == '"' then
        val foundIndex = parseObjectKey(fieldNameMatrix)
        Some(foundIndex)
      else if tt == '}' then None
      else throw new JsonParseError(s"Expected object field name or '}' but found '$tt'", this)
    else if t == 'n' then
      readChars(JsonSource.ull, "null")
      null
    else
      backspace()
      throw new JsonParseError(s"Expected object start '{' or null", this)

  def expectObjectField(fieldNameMatrix: StringMatrix): Option[Int] =
    val t = readToken()
    if t == ',' then
      val tt = readToken()
      if tt == '"' then
        val foundIndex = parseObjectKey(fieldNameMatrix)
        Some(foundIndex)
      else throw new JsonParseError(s"Expected object field name but found '$tt'", this)
    else if t == '}' then None
    else
      backspace()
      throw new JsonParseError(s"Expected ',' or '}' but found '$t'", this)

  final def parseObjectKey(fieldNameMatrix: StringMatrix): Int = // returns index of field name or -1 if not found
    var fi: Int = 0
    var bs: Long = fieldNameMatrix.initial
    var c: Int = here
    while c != '"' do {
      bs = fieldNameMatrix.update(bs, fi, c)
      fi += 1
      i += 1
      c = here
    }
    i += 1
    bs = fieldNameMatrix.exact(bs, fi)
    if readToken() != ':' then throw new JsonParseError(s"Expected ':' field separator but found $here", this)
    val ret = fieldNameMatrix.first(bs)
    ret

  @tailrec
  final def parseMap[K, V](kf: () => K, vf: () => V, acc: Map[K, V], isFirst: Boolean = true): Map[K, V] = // initial '{' already consumed
    readToken() match
      case '}' => acc
      case ',' =>
        val key = kf()
        expectToken(':')
        val value = vf()
        parseMap[K, V](kf, vf, acc + (key -> value), false)
      case _ if isFirst =>
        backspace()
        val key = kf()
        expectToken(':')
        val value = vf()
        parseMap[K, V](kf, vf, acc + (key -> value), false)
      case c => throw JsonParseError(s"Expected either object end '}' or field separator ',' here but got '$c'", this)

  @tailrec
  final def parseOrderedMap[K, V](kf: () => K, vf: () => V, acc: scala.collection.mutable.LinkedHashMap[K, V], isFirst: Boolean = true): scala.collection.mutable.LinkedHashMap[K, V] = // initial '{' already consumed
    readToken() match
      case '}' => acc
      case ',' =>
        val key = kf()
        expectToken(':')
        val value = vf()
        acc(key) = value
        parseOrderedMap[K, V](kf, vf, acc, false)
      case _ if isFirst =>
        backspace()
        val key = kf()
        expectToken(':')
        val value = vf()
        acc(key) = value
        parseOrderedMap[K, V](kf, vf, acc, false)
      case c => throw JsonParseError(s"Expected either object end '}' or field separator ',' here but got '$c'", this)

  @tailrec
  final def findAllFieldNames(acc: List[String] = List.empty[String]): List[String] =
    val mark = i
    expectToken('{')
    readToken() match
      case '}' =>
        i = mark
        acc
      case '"' =>
        val endI = parseString(i)
        val str = js.subSequence(i, endI).toString
        i = endI + 1
        expectToken(':')
        skipValue()
        if readToken() == '}' then backspace() // else consume ','
        findAllFieldNames(acc :+ str)
      case t =>
        backspace()
        throw JsonParseError(s"Expected either string start '\"' or object end '}' but got '$t'", this)

  @tailrec
  final def findObjectField(fieldName: String): Option[String] =
    val mark = i
    expectToken('{')
    readToken() match
      case '}' =>
        i = mark
        None
      case '"' =>
        val endI = parseString(i)
        val str = js.subSequence(i, endI).toString
        i = endI + 1
        expectToken(':')
        if str == fieldName then
          val found = Some(expectString())
          i = mark
          found
        else
          skipValue()
          if readToken() == '}' then backspace() // else consume ','
          findObjectField(fieldName)
      case t =>
        backspace()
        throw JsonParseError(s"Expected either string start '\"' or object end '}' but got '$t'", this)

  // Array and Tuple...
  // =======================================================

  @tailrec
  final private def addAllArray[E](s: scala.collection.mutable.ListBuffer[E], f: () => E, isFirst: Boolean): scala.collection.mutable.ListBuffer[E] =
    if i == max then throw JsonParseError("Unexpected end of buffer", this)
    val tt = readToken()
    if tt == ']' then s
    else if !isFirst && tt != ',' then throw JsonParseError(s"Expected ',' or ']' got '$tt'", this)
    else
      if isFirst then backspace()
      s.addOne(f())
      addAllArray(s, f, false)

  def expectArray[E](f: () => E): scala.collection.mutable.ListBuffer[E] =
    val t = readToken()
    if t == '[' then
      val seq = scala.collection.mutable.ListBuffer.empty[E]
      addAllArray(seq, f, true)
      seq
    else if t == 'n' then
      readChars(JsonSource.ull, "null")
      null
    else throw JsonParseError(s"Expected array start '[' or null but got '$t'", this)

  // String...
  // =======================================================

  // Value might be null!
  // expectString() will look for leading '"'.  parseString() presumes the '"' has already been consumed.
  inline def expectString(): String =
    val mark = i
    val t = readToken()
    if t == '"' then
      val endI = parseString(i)
      if endI >= 0 then
        val str = js.subSequence(i, endI).toString
        i = endI + 1
        str
      else // slower-parseString looking for escaped special chars
        val buf = FastStringBuilder()
        expectEncodedString(buf)
        buf.result
    else if t == 'n' then
      readChars(JsonSource.ull, "null")
      null
    else
      i = mark
      throw new JsonParseError(s"Expected a String value but got '$t'", this)

  @tailrec
  final private def expectEncodedString(buf: FastStringBuilder): Unit =
    readChar() match
      case '"' => () // done
      case '\\' =>
        readChar() match
          case '"' =>
            buf.append('\"')
            expectEncodedString(buf)
          case '\\' =>
            buf.append('\\')
            expectEncodedString(buf)
          case 'b' =>
            buf.append('\b')
            expectEncodedString(buf)
          case 'f' =>
            buf.append('\f')
            expectEncodedString(buf)
          case 'n' =>
            buf.append('\n')
            expectEncodedString(buf)
          case 'r' =>
            buf.append('\r')
            expectEncodedString(buf)
          case 't' =>
            buf.append('\t')
            expectEncodedString(buf)
          case 'u' =>
            val hexEncoded = js.subSequence(i, i + 4)
            i = i + 4
            val unicodeChar = Integer.parseInt(hexEncoded.toString, 16).toChar
            buf.append(unicodeChar.toString)
            expectEncodedString(buf)
          case c =>
            buf.append(c)
            expectEncodedString(buf)
      case c =>
        buf.append(c)
        expectEncodedString(buf)

  def expectString[T](parseFn: String => T): T =
    expectString() match
      case s: String => parseFn(s)
      case null      => null.asInstanceOf[T]

  @tailrec
  final def parseString(pos: Int): Int =
    if pos + 3 < max then // Based on SWAR routine of JSON string parsing: https://github.com/sirthias/borer/blob/fde9d1ce674d151b0fee1dd0c2565020c3f6633a/core/src/main/scala/io/bullet/borer/json/JsonParser.scala#L456
      val bs = (js.charAt(pos)) | (js.charAt(pos + 1) << 8) | (js.charAt(pos + 2) << 16) | js.charAt(pos + 3) << 24
      val mask = ((bs - 0x20202020 ^ 0x3c3c3c3c) - 0x1010101 | (bs ^ 0x5d5d5d5d) + 0x1010101) & 0x80808080
      if mask != 0 then {
        val offset = java.lang.Integer.numberOfTrailingZeros(mask) >> 3
        if (bs >> (offset << 3)).toByte == '"' then pos + offset
        else -1 // special char found
      } else parseString(pos + 4)
    else if pos == max then throw new Exception("Unterminated string value")
    else
      val b = js.charAt(pos)
      if b == '"' then pos
      else if (b - 0x20 ^ 0x3c) <= 0 then -1 // special char found
      else parseString(pos + 1)

  // Boolean...
  // =======================================================

  def expectBoolean(): Boolean =
    skipWS()
    val bs = (js.charAt(pos)) | (js.charAt(pos + 1) << 8) | (js.charAt(pos + 2) << 16) | js.charAt(pos + 3) << 24
    if (bs ^ JsonSource.trueBytes) == 0 then
      i += 4
      true
    else if pos + 4 < max && ((bs | js.charAt(pos + 4)) ^ JsonSource.falseBytes) == 0 then
      i += 5
      false
    else throw JsonParseError(s"Expected 'true' or 'false'", this)

  // Java Boolean can be null
  def expectJavaBoolean(): java.lang.Boolean =
    skipWS()
    val bs = (js.charAt(pos)) | (js.charAt(pos + 1) << 8) | (js.charAt(pos + 2) << 16) | js.charAt(pos + 3) << 24
    if (bs ^ JsonSource.trueBytes) == 0 then
      i += 4
      java.lang.Boolean.TRUE
    else if pos + 4 < max && ((bs | js.charAt(pos + 4)) ^ JsonSource.falseBytes) == 0 then
      i += 5
      java.lang.Boolean.FALSE
    else if readChar() == 'n' then
      readChars(JsonSource.ull, "null")
      null.asInstanceOf[java.lang.Boolean]
    else
      backspace()
      throw JsonParseError(s"Expected 'true', 'false', or null here", this)

  // Characters...
  // =======================================================

  private var c: Char = 0
  inline def readChar(): Char =
    if i < max then
      c = here
      i += 1
      c
    else BUFFER_EXCEEDED

  inline def readChars(
      expect: Array[Char],
      errMsg: String
  ): Unit =
    var i: Int = 0
    while i < expect.length do
      if readChar() != expect(i) then throw JsonParseError(s"Expected $errMsg", this)
      i += 1

  inline def expectChar(): Char =
    expectString() match {
      case s if s.length == 1 => s.charAt(0)
      case s                  => throw new JsonParseError(s"Expected a Char value but got '$s'", this)
    }

  // Numbers...
  // =======================================================

  @inline private def isNumber(c: Char): Boolean =
    (c: @switch) match
      case '+' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | 'e' | 'E' => true
      case _                                                                                       => false

  def expectFloat(): Float =
    val result = UnsafeNumbers.float_(this, false, 32)
    backspace()
    result

  def expectDouble(): Double =
    val result = UnsafeNumbers.double_(this, false, 64)
    backspace()
    result

  def expectNumberOrNull(): String =
    skipWS()
    val mark = i
    skipNumber()
    if i != mark then captureMark(mark)
    else if readChar() == 'n' then
      readChars(JsonSource.ull, "null")
      null
    else
      backspace()
      throw new JsonParseError("Expected a numerical value or null here", this)

  def expectInt(): Int =
    var b = readToken()
    var s = -1
    if b == '-' then
      b = readChar()
      s = 0
    if b < '0' || b > '9' then
      backspace()
      throw JsonParseError("Non-numeric character found when integer value expected", this)
    var x = '0' - b
    while { b = readChar(); b >= '0' && b <= '9' } do
      if x < -214748364 || {
          x = x * 10 + ('0' - b)
          x > 0
        }
      then throw JsonParseError("Integer value overflow", this)
    x ^= s
    x -= s
    if (s & x) == -2147483648 then throw JsonParseError("Integer value overflow", this)
    if (b | 0x20) == 'e' || b == '.' then
      backspace()
      throw JsonParseError("Decimal digit 'e' or '.' found when integer value expected", this)
    backspace()
    x

  def expectLong(): Long =
    val result = UnsafeNumbers.long_(this, false)
    backspace()
    result

  // Skip things...
  // =======================================================

  inline def skipWS(): Unit =
    while (here == ' ' || here == '\n' || (here | 0x4) == '\r' || here == '\t') && i < max do i += 1
    if i == max then throw new JsonParseError("Unexpected end of buffer", this)

  def skipValue(): Unit =
    (readToken(): @switch) match {
      case 'n' => readChars(JsonSource.ull, "null")
      case 'f' => readChars(JsonSource.alse, "false")
      case 't' => readChars(JsonSource.rue, "true")
      case '{' => skipObjectValue()
      case '[' => skipArrayValue()
      case '"' =>
        i += 1
        val endI = parseString(i)
        i = endI + 1
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' =>
        skipNumber()
      case c => throw JsonParseError(s"Unexpected '$c'", this)
    }

  @tailrec
  final def skipNumber(): Unit =
    if !isNumber(readChar()) then backspace()
    else skipNumber()

  @tailrec
  final def skipArrayValue(k: Int = 0): Unit =
    readChar() match
      case ']' if k == 0 => ()
      case '"' =>
        i = parseString(i) + 1
        skipArrayValue(k)
      case ']' => skipArrayValue(k - 1)
      case '[' => skipArrayValue(k + 1)
      case _   => skipArrayValue(k)

  @tailrec
  final def skipObjectValue(k: Int = 0): Unit =
    readChar() match
      case '}' if k == 0 => ()
      case '"' =>
        i = parseString(i) + 1
        skipObjectValue(k)
      case '}' => skipObjectValue(k - 1)
      case '{' => skipObjectValue(k + 1)
      case _   => skipObjectValue(k)
