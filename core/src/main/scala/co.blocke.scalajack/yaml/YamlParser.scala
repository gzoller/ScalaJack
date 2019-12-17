package co.blocke.scalajack
package yaml

import model._
import typeadapter.ClassTypeAdapterBase

import scala.collection.mutable
import scala.reflect.runtime.universe.Type
import scala.jdk.CollectionConverters._

case class YamlParser(input: YAML, jackFlavor: JackFlavor[YAML])
    extends Parser {

  type WIRE = YAML

  private val LIST_PREFIX = "- "
  private val MAP_PREFIX = ": "
  private val QUEST_PREFIX = "? "

  private var line = 0
  private val yaml: Array[Char] = input.toCharArray
  private var i = 0
  private val max: Int = yaml.length
  private var tabStack = mutable.Stack[Int](0)
  private var savedStack: mutable.Stack[Int] = mutable.Stack.empty[Int]

  @inline def whitespace(): Boolean = {
    while (i < max && yaml(i).isWhitespace) i += 1
    true
  }
  @inline def nullCheck(): Boolean =
    yaml(i) == 'n' && i + 4 <= max && input.substring(i, i + 4) == "null"
  @inline def skipToEOL(): Unit =
    if (i < max) {
      while (i < max && yaml(i) != '\n') i += 1
      i += 1
      incLine()
    }
  @inline def incLine(): Unit = line += 1
  @inline def pushTab(t: Int): Unit =
    if (t > tabStack.head) tabStack.push(t)
    else
      throw new ScalaJackError(showError("Bad tab indent " + t))

  def backspace(): Unit = i -= 1

  // Leaves cursor on first non-tab character, i.e. ready to parse the next thing
  private def discernTabNextLine: Int = {
    skipToEOL()
    var tab = 0
    val mark = i
    while (i < max && "\n# ".contains(yaml(i))) yaml(i) match {
      case '#' | '\n' =>
        tab = 0
        skipToEOL()
      case ' ' =>
        tab += 1
        i += 1
      case _ =>
    }
    i = mark
    tab
  }

  private def expectIndent(): Boolean = {
    val expected = tabStack.head
    if (i + expected < max && input.substring(i, i + expected) == " " * expected) {
      i += expected
      true
    } else false
  }

  def expectString(nullOK: Boolean = true): String = {
    val readString = (cond: () => Boolean) => {
      val mark = i
      var hasSpecialChar = false
      while (i < max && cond()) {
        if (yaml(i) == '\\') {
          hasSpecialChar = true
          i += 1
        }
        i += 1
      }
      if (hasSpecialChar)
        _expectString(mark, i)
      else
        input.substring(mark, i)
    }
    val readNLString = (nlReplace: String) => {
      pushTab(discernTabNextLine)
      val builder = new java.lang.StringBuilder()
      while (expectIndent()) {
        builder.append(readString(() => yaml(i) != '\n'))
        val c = yaml(i)
        if (i < max && yaml(i) == '\n') {
          builder.append(nlReplace)
          i += 1
          incLine()
        }
      }
      i -= 1 // backspace over \n because caller does a skipToEOL()
      tabStack.pop()
      builder.toString
    }

    val strVal = if (nullCheck()) {
      if (nullOK) {
        i += 4
        null
      } else
        throw new ScalaJackError(showError("Expected a String here"))
    } else
      yaml(i) match {
        case '"' =>
          i += 1
          readString(() => yaml(i) != '"')
        case '|' =>
          readNLString("\n")
        case '>' =>
          readNLString(" ")
        case _ =>
          readString(() => yaml(i) != '\n' && yaml(i) != '#')
      }
    skipToEOL()
    strVal.stripTrailing
  }

  // Slower "capture" version for when we discover embedded special chars that need translating, i.e. we
  // can't do a simple substring within quotes.
  private def _expectString(start: Int, end: Int): String = {
    val builder = new java.lang.StringBuilder()
    i = start
    while (i <= end) {
      if (yaml(i) == '\\') {
        yaml(i + 1) match {
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
            val hexEncoded = input.substring(i + 2, i + 6)
            val unicodeChar = Integer.parseInt(hexEncoded, 16).toChar
            builder.append(unicodeChar.toString)
            i += 6

          case c =>
            builder.append('\\') // unrecognized special char
            builder.append(c)
            i += 2
        }
      } else {
        builder.append(yaml(i))
        i += 1
      }
    }
    builder.toString
  }

  def expectBoolean(): Boolean = {
    val b = if (i + 4 <= max && input.substring(i, i + 4) == "true") {
      i += 4
      true
    } else if (i + 5 <= max && input.substring(i, i + 5) == "false") {
      i += 5
      false
    } else
      throw new ScalaJackError(showError("Expected a Boolean here"))
    skipToEOL()
    b
  }

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '-' || char == '+'

  def expectNumber(nullOK: Boolean = false): String = {
    val mark = i
    val num = nullCheck() match {
      case true if nullOK =>
        i += 4
        null
      case true =>
        throw new ScalaJackError(showError("Expected a Number here"))
      case false =>
        while (i < max && isNumberChar(yaml(i))) i += 1
        if (mark == i)
          throw new ScalaJackError(showError("Expected a Number here"))
        else if (i == max || "\t\n #".contains(yaml(i)))
          input.substring(mark, i)
        else
          throw new ScalaJackError(showError("Expected a Number here"))
    }
    skipToEOL()
    num
  }

  def expectList[E, TO](elemTypeAdapter: TypeAdapter[E],
                        builder: mutable.Builder[E, TO]): TO = {
    if (!expectIndent())
      throw new ScalaJackError(
        showError(
          s"Didn't find expected number of indent spaces (${tabStack.head}})."
        )
      )
    if (i + 2 >= max || input.substring(i, i + 2) != LIST_PREFIX)
      throw new ScalaJackError(showError("Expected start of list here"))

    // TODO: This is too simple--scalars basically.  Must account for list of collections and flow notation [a,b,c]
    i += 2 // skip list prefix
    builder += elemTypeAdapter.read(this) // Parse next item

    var mark = i
    while (expectIndent() && i + 2 < max && input.substring(i, i + 2) == LIST_PREFIX) {
      i += 2
      builder += elemTypeAdapter.read(this) // Parse next item
      mark = i
    }
    if (i < max) {
      i = mark // backtrack if different indent level found
      tabStack.pop()
    }
    builder.result()
  }

  def expectTuple(
    readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] = {
    /*
    if (i == max || yaml(i) != '[')
      throw new ScalaJackError(showError("Expected start of tuple here"))
    i += 1
    var first = true
    val result = readFns.map { fn =>
      whitespace()
      if (!first) {
        if (i == max || yaml(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      fn.valueTypeAdapter.read(this)
    }
    if (i == max || yaml(i) != ']')
      throw new ScalaJackError(showError("Expected end of tuple here"))
    i += 1
    result

     */
    null.asInstanceOf[List[Any]]
  }

  def expectMap[K, V, TO](keyTypeAdapter: TypeAdapter[K],
                          valueTypeAdapter: TypeAdapter[V],
                          builder: mutable.Builder[(K, V), TO]): TO = {
    /*
    whitespace()
    if (yaml(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1
    var first = true
    while (i < max && yaml(i) != '}') {
      whitespace()
      if (!first) {
        if (i == max || yaml(i) != ',')
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
      if (i == max || yaml(i) != ':')
        throw new ScalaJackError(showError("Expected colon here"))
      i += 1
      whitespace()
      val value = valueTypeAdapter.read(this)
      whitespace()
      builder += ((key, value))
    }
    if (i == max || yaml(i) != '}')
      throw new ScalaJackError(showError("Expected end of object here"))
    i += 1
    builder.result()
     */
    null.asInstanceOf[TO]
  }

  def expectObject(
    classBase: ClassTypeAdapterBase[_],
    hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, _]) = {
    /*
    whitespace()
    val args = classBase.argsTemplate.clone()
    val fieldBits = classBase.fieldBitsTemplate.clone()
    val captured =
      if (classBase.isSJCapture) new java.util.HashMap[String, String]()
      else null
    if (i == max || yaml(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1
    var first = true
    while (i < max && yaml(i) != '}') {
      whitespace()
      if (!first) {
        if (i == max || yaml(i) != ',')
          throw new ScalaJackError(showError("Expected comma here"))
        else
          i += 1 // skip ','
        whitespace()
      } else
        first = false
      val key = expectString(false)
      whitespace()
      if (i == max || yaml(i) != ':')
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
    if (i == max || yaml(i) != '}')
      throw new ScalaJackError(showError("Expected end of object here"))
    i += 1
    (fieldBits, args, captured)
     */
    null
      .asInstanceOf[(mutable.BitSet, Array[Any], java.util.HashMap[String, _])]
  }

  private def skipString(): Unit = {
    i += 1
    while (i < max && yaml(i) != '"') {
      if (yaml(i) == '\\') i += 1
      i += 1
    }
    i += 1
  }

  private def skipOverElement(): Unit = {
    /*
    whitespace()
    yaml(i) match {
      case '[' =>
        var level = 0
        i += 1
        while (i < max && level >= 0) {
          yaml(i) match {
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
        while (i < max && level >= 0) yaml(i) match {
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
        while (i < max && yaml(i) != ',' && yaml(i) != '}' && yaml(i) != ']') i += 1
    }
   */
  }

  def peekForNull: Boolean =
    if (nullCheck()) {
      i += 4
      true
    } else false

  // NOTE: Expectation here is we're sitting on beginning of object, '{'.  This is called from TraitTypeAdapter
  def scanForHint(hint: String, converterFn: HintBijective): Type = {
    /*
    val mark = i
    whitespace()
    if (i == max || yaml(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    i += 1 // skip over {
    var done = false
    while (!done) {
      whitespace()
      val key = expectString()
      whitespace()
      if (i == max || yaml(i) != ':')
        throw new ScalaJackError(showError("Expected ':' here"))
      i += 1 // skip ':'
      if (key == hint) {
        whitespace()
        done = true
      } else {
        skipOverElement()
        whitespace()
        yaml(i) match {
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
     */
    null.asInstanceOf[Type]
  }

  def resolveTypeMembers(
    typeMembersByName: Map[String, ClassHelper.TypeMember[_]],
    converterFn: HintBijective
  ): Map[Type, Type] = {
    /*
    val mark = i
    whitespace()
    if (i == max || yaml(i) != '{')
      throw new ScalaJackError(showError("Expected start of object here"))
    val collected = new java.util.HashMap[Type, Type]()
    i += 1 // skip over {
    var done = false
    while (!done) {
      whitespace()
      val key = expectString()
      whitespace()
      if (i == max || yaml(i) != ':')
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
      yaml(i) match {
        case ',' => i += 1 // skip ','
        case '}' => done = true
        case _ =>
          throw new ScalaJackError(showError("Unexpected character found"))
      }
    }
    i = mark // go back to parse object
    collected.asScala.toMap
     */
    null.asInstanceOf[Map[Type, Type]]
  }

  // TODO: This has to be MUCH smarter!  i.e. handle newlines
  private def lineContainsColon: Boolean = {
    while (i < max && yaml(i) != ':') i += 1
    !(i == max)
  }
  // TODO: This has to be MUCH smarter!  i.e. handle newlines
  private def lineContainsDash: Boolean = {
    while (i < max && yaml(i) != '-') i += 1
    !(i == max)
  }

  def showError(msg: String): String = s"Line $line: $msg"

  def mark(): Int = {
    savedStack = tabStack
    i
  }
  def revertToMark(mark: Int): Unit = {
    i = mark
    tabStack = savedStack
  }

  def nextIsString: Boolean =
    true // basically everything in YAML is a string if its not something else
  def nextIsNumber: Boolean = whitespace() && isNumberChar(yaml(i))
  def nextIsObject: Boolean = whitespace() && (nullCheck() || lineContainsColon)
  def nextIsArray: Boolean = whitespace() && (nullCheck() || lineContainsDash)
  def nextIsBoolean: Boolean =
    whitespace() && ((i + 4 <= max && input.substring(i, i + 4) == "true") || (i + 5 <= max && input
      .substring(i, i + 5) == "false"))

  def sourceAsString: String = input

  def subParser(input: YAML): Parser = YamlParser(input, jackFlavor)
}
