package co.blocke.scalajack
package json

import model._
import typeadapter.ClassTypeAdapterBase

import scala.collection.mutable
import scala.reflect.runtime.universe.Type
import scala.collection.JavaConverters._

case class JsonParser(js: JSON, jackFlavor: JackFlavor[JSON]) extends Parser {

  type WIRE = JSON

  private val jsChars: Array[Char] = js.toCharArray
  private var i = 0
  private val max: Int = jsChars.length

  @inline def whitespace(): Unit =
    while (i < max && jsChars(i).isWhitespace) i += 1
  @inline def nullCheck(): Boolean =
    jsChars(i) == 'n' && i + 4 <= max && js.substring(i, i + 4) == "null"

  def backspace(): Unit = i -= 1

  def expectString(nullOK: Boolean = true): String = {
    if (nullOK && nullCheck()) {
      i += 4
      null
    } else if (jsChars(i) == '"') {
      i += 1
      val mark = i
      var captured: Option[String] = None
      while (i < max && jsChars(i) != '"') {
        if (jsChars(i) == '\\') { // Oops!  Special char found.  Reset and try again while capturing/translating special chars
          i = mark
          captured = Some(_expectString())
        } else
          i += 1
      }
      i += 1
      captured.getOrElse(js.substring(mark, i - 1))
    } else
      throw new ScalaJackError(showError("Expected a String here"))
  }

  // Slower "capture" version for when we discover embedded special chars that need translating, i.e. we
  // can't do a simple substring within quotes.
  private def _expectString(): String = {
    val builder = new java.lang.StringBuilder()
    while (i < max && jsChars(i) != '"') {
      if (jsChars(i) == '\\') {
        jsChars(i + 1) match {
          case '"' =>
            builder.append('\"')
            i += 2

          case '\\' =>
            builder.append('\\')
            i += 2

          case 'b' =>
            builder.append('\b')
            i += 2

          case 'f' =>
            builder.append('\f')
            i += 2

          case 'n' =>
            builder.append('\n')
            i += 2

          case 'r' =>
            builder.append('\r')
            i += 2

          case 't' =>
            builder.append('\t')
            i += 2

          case 'u' =>
            val hexEncoded = js.substring(i + 2, i + 6)
            val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
            builder.append(unicodeChar.toString)
            i += 6

          case c =>
            builder.append(c)
            i += 2
        }
      } else {
        builder.append(jsChars(i))
        i += 1
      }
    }
    builder.toString
  }

  def expectBoolean(): Boolean =
    if (i + 4 <= max && js.substring(i, i + 4) == "true") {
      i += 4
      true
    } else if (i + 5 <= max && js.substring(i, i + 5) == "false") {
      i += 5
      false
    } else
      throw new ScalaJackError(showError("Expected a Boolean here"))

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '-' || char == '+'

  def expectNumber(): String = {
    val mark = i
    while (i < max && isNumberChar(jsChars(i))) i += 1
    if (mark == i) {
      if (nullCheck()) {
        i += 4
        null
      } else
        throw new ScalaJackError(showError("Expected a Number here"))
    } else if (i == max || " ,}]".contains(jsChars(i)))
      js.substring(mark, i)
    else
      throw new ScalaJackError(showError("Expected a Number here"))
  }

  def expectList[E, TO](
      elemTypeAdapter: TypeAdapter[E],
      builder:         mutable.Builder[E, TO]): TO = {
    if (jsChars(i) != '[')
      throw new ScalaJackError(showError("Expected start of list here"))
    i += 1
    var first = true
    while (i < max && jsChars(i) != ']') {
      whitespace()
      if (!first) {
        if (jsChars(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      builder += elemTypeAdapter.read(this) // Parse next item!
      whitespace()
    }
    if (i == max || jsChars(i) != ']')
      throw new ScalaJackError(showError("Expected end of list here"))
    i += 1
    builder.result()
  }

  def expectTuple(
      readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] = {
    if (i == max || jsChars(i) != '[')
      throw new ScalaJackError(showError("Expected start of tuple here"))
    i += 1
    var first = true
    val result = readFns.map { fn =>
      whitespace()
      if (!first) {
        if (i == max || jsChars(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      fn.valueTypeAdapter.read(this)
    }
    if (i == max || jsChars(i) != ']')
      throw new ScalaJackError(showError("Expected end of tuple here"))
    i += 1
    result
  }

  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO = {
    whitespace()
    if (jsChars(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1
    var first = true
    while (i < max && jsChars(i) != '}') {
      whitespace()
      if (!first) {
        if (i == max || jsChars(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      val key = keyTypeAdapter.read(this)
      if (key == null)
        throw new ScalaJackError(showError("Map keys cannot be null"))
      whitespace()
      if (i == max || jsChars(i) != ':')
        throw new ScalaJackError(showError("Expected colon here"))
      i += 1
      whitespace()
      val value = valueTypeAdapter.read(this)
      whitespace()
      builder += ((key, value))
    }
    if (i == max || jsChars(i) != '}')
      throw new ScalaJackError(showError("Expected end of object here"))
    i += 1
    builder.result()
  }

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, _]) = {
    whitespace()
    val args = classBase.argsTemplate.clone()
    val fieldBits = classBase.fieldBitsTemplate.clone()
    val captured =
      if (classBase.isSJCapture) new java.util.HashMap[String, String]()
      else null
    if (i == max || jsChars(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1
    var first = true
    while (i < max && jsChars(i) != '}') {
      whitespace()
      if (!first) {
        if (i == max || jsChars(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      val key = expectString(false)
      whitespace()
      if (i == max || jsChars(i) != ':')
        throw new ScalaJackError(showError("Expected colon here"))
      i += 1
      classBase.fieldMembersByName
        .get(key)
        .map { field =>
          whitespace()
          fieldBits -= field.index
          args(field.index) = field.valueTypeAdapter.read(this)
        }
        .getOrElse {
          val mark = i
          skipOverElement()
          if (classBase.isSJCapture && key != hintLabel)
            captured.put(key, js.substring(mark, i))
        }
      whitespace()
    }
    if (i == max || jsChars(i) != '}')
      throw new ScalaJackError(showError("Expected end of object here"))
    i += 1
    (fieldBits, args, captured)
  }

  private def skipString(): Unit = {
    i += 1
    while (i < max && jsChars(i) != '"') {
      if (jsChars(i) == '\\') i += 1
      i += 1
    }
    i += 1
  }

  def skipOverElement(): Unit = {
    whitespace()
    jsChars(i) match {
      case '[' =>
        var level = 0
        i += 1
        while (i < max && level >= 0) {
          jsChars(i) match {
            case '[' =>
              level += 1
              i += 1
            case '"' => skipString()
            case ']' =>
              level -= 1
              i += 1
            case _ =>
              i += 1
          }
        }
      case '{' =>
        var level = 0
        i += 1
        while (i < max && level >= 0) jsChars(i) match {
          case '{' =>
            level += 1
            i += 1
          case '"' => skipString()
          case '}' =>
            level -= 1
            i += 1
          case _ =>
            i += 1
        }
      case '"' => skipString()
      case _ => // "naked" value: null, number, boolean
        while (i < max && jsChars(i) != ',' && jsChars(i) != '}' && jsChars(i) != ']') i += 1
    }
  }

  def peekForNull: Boolean =
    if (nullCheck()) {
      i += 4
      true
    } else false

  // NOTE: Expectation here is we're sitting on beginning of object, '{'.  This is called from TraitTypeAdapter
  def scanForHint(hint: String, converterFn: HintBijective): Type = {
    val mark = i
    whitespace()
    if (i == max || jsChars(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1 // skip over {
    var done = false
    while (!done) {
      whitespace()
      val key = expectString()
      whitespace()
      if (i == max || jsChars(i) != ':')
        throw new ScalaJackError(showError("Expected ':' here"))
      i += 1 // skip ':'
      if (key == hint) {
        whitespace()
        done = true
      } else {
        skipOverElement()
        whitespace()
        jsChars(i) match {
          case ',' => i += 1 // skip ','
          case '}' =>
            throw new ScalaJackError(showError(s"Type hint '$hint' not found"))
          case _ =>
            throw new ScalaJackError(showError("Unexpected character found"))
        }
      }
    }
    val rawHintString = expectString()
    val hintType = try {
      converterFn.apply(rawHintString)
    } catch {
      case t: Throwable =>
        i -= 1
        throw new ScalaJackError(
          showError(s"Couldn't marshal class for $rawHintString")
        )
    }
    i = mark // we found hint, but go back to parse object
    hintType
  }

  def resolveTypeMembers(
      typeMembersByName: Map[String, ClassHelper.TypeMember[_]],
      converterFn:       HintBijective
  ): Map[Type, Type] = {
    val mark = i
    whitespace()
    if (i == max || jsChars(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    val collected = new java.util.HashMap[Type, Type]()
    i += 1 // skip over {
    var done = false
    while (!done) {
      whitespace()
      val key = expectString()
      whitespace()
      if (i == max || jsChars(i) != ':')
        throw new ScalaJackError(showError("Expected ':' here"))
      i += 1 // skip ':'
      if (typeMembersByName.contains(key)) {
        whitespace()
        collected.put(
          typeMembersByName(key).typeSignature,
          converterFn.apply(expectString())
        )
      } else
        skipOverElement()
      whitespace()
      jsChars(i) match {
        case ',' => i += 1 // skip ','
        case '}' => done = true
        case _ =>
          throw new ScalaJackError(showError("Unexpected character found"))
      }
    }
    i = mark // go back to parse object
    collected.asScala.toMap
  }

  def showError(msg: String): String = {
    val (clip, dashes) = i match {
      case ep if ep <= 50 && max < 80 => (js, ep)
      case ep if ep <= 50             => (js.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= max =>
        ("..." + js.substring(i - 49), 52)
      case ep => ("..." + js.substring(ep - 49, ep + 27) + "...", 52)
    }
    msg + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }

  def mark(): Int = i
  def revertToMark(mark: Int): Unit = i = mark

  def nextIsString: Boolean = nullCheck() || jsChars(i) == '"'
  def nextIsNumber: Boolean = isNumberChar(jsChars(i))
  def nextIsObject: Boolean = nullCheck() || jsChars(i) == '{'
  def nextIsArray: Boolean = nullCheck() || jsChars(i) == '['
  def nextIsBoolean: Boolean =
    (i + 4 <= max && js.substring(i, i + 4) == "true") || (i + 5 <= max && js
      .substring(i, i + 5) == "false")
  def sourceAsString: String = js

  def subParser(input: JSON): Parser = JsonParser(input, jackFlavor)
}
