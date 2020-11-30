package co.blocke.scalajack
package yaml

import model._
import typeadapter.classes.ClassTypeAdapterBase
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.TypeMemberInfo

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader
import org.snakeyaml.engine.v2.api.LoadSettings
import org.snakeyaml.engine.v2.events._

case class YamlParser(input: YAML, jackFlavor: JackFlavor[YAML]) extends Parser {

  private val loadSettings = LoadSettings.builder().build()
  private val snake = new ParserImpl(new StreamReader(input.asInstanceOf[String], loadSettings), loadSettings)

  private var indentLevel = 0

  private def doICare(e: Event) = e match {
    case _: StreamStartEvent   => false
    case _: StreamEndEvent     => false
    case _: DocumentStartEvent => false
    case _: DocumentEndEvent   => false
    case _                     => true
  }

  type WIRE = YAML

  // Just the Events I care about
  private val events = snake.asScala.filter(doICare).toList

  private val indentLevelMap = mutable.Map.empty[Int, Int]

  private var i = 0

  def expectString(nullOK: Boolean = true): String = {
    events(i) match {
      case s: ScalarEvent =>
        i += 1
        s.getScalarStyle.toString match {
          case "|" | ">"                           => s.getValue().trim()
          case _ if s.getValue == "null" && nullOK => null
          case _                                   => s.getValue
        }
      case e =>
        throw new ScalaJackError(showError("Expected a String here: " + e))
    }
  }

  private def expectCollecton[K, TO](nextTest: Boolean, builder: mutable.Builder[K, TO], fn: () => Unit, errStr: String): TO = {
    indentLevel += 1
    val startIndent = indentLevel
    if (!nextTest)
      throw new ScalaJackError(showError(errStr + events(i)))
    i += 1
    while (!events(i).isInstanceOf[CollectionEndEvent] || indentLevel != startIndent) 
      fn()
    i += 1 // consume collection end event
    indentLevel -= 1
    builder.result()
  }

  def expectList[K, TO](elemTypeAdapter: TypeAdapter[K], builder: mutable.Builder[K, TO]): TO =
    expectCollecton(nextIsArray, builder, () => {
      builder += elemTypeAdapter.read(this)
    }, "Expected a List here: ")

  def expectTuple(
    tupleFieldTypeAdapters: List[TypeAdapter[_]]
  ): List[Object] = {
    val builder = new mutable.ListBuffer[Object]()
    expectCollecton(nextIsArray, builder, () => {
      tupleFieldTypeAdapters.foreach { fieldTypeAdapter =>
        builder += fieldTypeAdapter.read(this).asInstanceOf[Object]
      }
    }, "Expected a Tuple here: ")
  }

  def expectMap[K, V, TO](keyTypeAdapter: TypeAdapter[K], valueTypeAdapter: TypeAdapter[V], builder: mutable.Builder[(K, V), TO]): TO =
    expectCollecton(
      nextIsObject,
      builder,
      () => {
        val mapKey   = keyTypeAdapter.read(this)
        val mapValue = valueTypeAdapter.read(this)
        builder += ((mapKey, mapValue))
      },
      "Expected a Map or Object here: "
    )

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, List[Object], java.util.HashMap[String, _]) = {
    indentLevel += 1
    val startIndent = indentLevel
    if (!nextIsObject)
      throw new ScalaJackError(showError("Expected an Object here: " + events(i)))
    i += 1

    val args      = classBase.argsTemplate.clone()
    val fieldBits = mutable.BitSet() 
    val captured =
      if (classBase.isSJCapture) new java.util.HashMap[String, List[Event]]()
      else null
    while (!events(i)
             .isInstanceOf[CollectionEndEvent] || indentLevel != startIndent) {
      val key = expectString(false)
      classBase.fieldMembersByName.get(key) match {
        case Some(field) =>
          fieldBits += field.info.index
          args(field.info.index) = field.valueTypeAdapter.read(this).asInstanceOf[Object]
        case None => // found some input field not present in class
          val mark = i
          skipOverElement(startIndent)
          if (classBase.isSJCapture && key != hintLabel)
            // This is weird but snakeyaml's internals expect a stream start/end event, so we need to
            // create these or boom happens.
            captured.put(key, new StreamStartEvent() +: events.slice(mark, i) :+ new StreamEndEvent())
      }
    }

    i += 1 // consume collection end event
    indentLevel -= 1
    (fieldBits, args.toList, captured)
  }

  def expectBoolean(): Boolean = events(i) match {
    case n: ScalarEvent if n.getValue == "true" =>
      i += 1
      true
    case n: ScalarEvent if n.getValue == "false" =>
      i += 1
      false
    case e =>
      throw new ScalaJackError(showError("Expected a Boolean value here: " + e))
  }

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '-' || char == '+'
  @inline def isNumber(s: String): Boolean = s.toCharArray.foldLeft(true) {
    case (acc, c) => acc && isNumberChar(c)
  }
  def expectNumber(nullOK: Boolean = false): String = events(i) match {
    case n: ScalarEvent if n.getValue == "null" || n.getValue == "" =>
      i += 1
      null
    case n: ScalarEvent if isNumber(n.getValue) =>
      i += 1
      n.getValue
    case e =>
      throw new ScalaJackError(showError("Expected a Number value here: " + e))
  }

  def peekForNull: Boolean = events(i) match {
    case n: ScalarEvent if n.getValue == "null" =>
      i += 1
      true
    case _ => false
  }

  def skipOverElement(startIndent: Int): Unit = {
    var done = false
    while (!done) {
      events(i) match {
        case _: ScalarEvent if indentLevel == startIndent =>
          done = true
        case _: CollectionStartEvent =>
          indentLevel += 1
        case _: CollectionEndEvent => // close of a nested collection
          indentLevel -= 1
          if (indentLevel == startIndent)
            done = true
        case _ =>
      }
      i += 1
    }
  }

  def scanForHint(hint: String, converterFn: HintBijective): Class[_] = {
    val mark = i
    indentLevel += 1
    val startIndent = indentLevel
    if (!nextIsObject)
      throw new ScalaJackError(
        showError("Expected an Object here: " + events(i))
      )
    i += 1

    val max   = events.length
    var found = false
    while (!found && i < max && !events(i).isInstanceOf[CollectionEndEvent]) {
      found = expectString(false) == hint
      if (!found)
        skipOverElement(startIndent)
    }
    if (!found)
      throw new ScalaJackError(showError(s"Type hint '$hint' not found"))

    // Found hint or we wouldn't be here
    val rawHintString = expectString(false)
    val hintType = try {
      converterFn.apply(rawHintString)
    } catch {
      case t: Throwable =>
        throw new ScalaJackError(
          showError(s"Couldn't marshal class for $rawHintString")
        )
    }
    i = mark // we found hint, but go back to parse object
    indentLevel -= 1
    Class.forName(hintType)
  }

  // For embedded type members.  Convert the type member into runtime "actual" type, e.g. T --> Foo
  def resolveTypeMembers(
      typeMembersByName: Map[String, TypeMemberInfo],
      converterFn: HintBijective
  ): Map[String, TypeMemberInfo] = {
    val mark = i
    indentLevel += 1
    val startIndent = indentLevel
    if (!nextIsObject)
      throw new ScalaJackError(
        showError("Expected an Object here: " + events(i))
      )
    i += 1

    val collected = new java.util.HashMap[String, TypeMemberInfo]()
    while (!events(i).isInstanceOf[CollectionEndEvent]) {
      val key = expectString()
      if (typeMembersByName.contains(key))
        collected.put(
          key,
          TypeMemberInfo(key, typeMembersByName(key).typeSymbol, RType.of(Class.forName(converterFn.apply(expectString()))))
        )
      else
        skipOverElement(startIndent)
    }
    i = mark // we found hint, but go back to parse object
    indentLevel -= 1
    collected.asScala.toMap
  }

  def showError(msg: String): String = s"Line ${events(i).getStartMark.get().getLine}: " + msg
  def backspace(): Unit              = i -= 1
  def mark(): Int = {
    indentLevelMap(i) = indentLevel
    i
  }
  def revertToMark(mark: Int): Unit = {
    i = mark
    indentLevel = indentLevelMap(i)
  }
  def nextIsString: Boolean = events(i).isInstanceOf[ScalarEvent]
  def nextIsNumber: Boolean =
    events(i).isInstanceOf[ScalarEvent] && isNumber(
      events(i).asInstanceOf[ScalarEvent].getValue
    )
  def nextIsObject: Boolean =
    events(i).isInstanceOf[CollectionStartEvent] && events(i).toString
      .startsWith("+MAP")
  def nextIsArray: Boolean =
    events(i).isInstanceOf[CollectionStartEvent] && events(i).toString
      .startsWith("+SEQ")
  def nextIsBoolean: Boolean =
    events(i).isInstanceOf[ScalarEvent] && List("true", "false").contains(
      events(i).asInstanceOf[ScalarEvent].getValue
    )
  // $COVERAGE-OFF$Unused, un-called by YamlFlavor machinery
  def subParser(input: YAML): Parser = this.copy(input = input)
  def sourceAsString: String         = input.asInstanceOf[String]
  // $COVERAGE-ON$
}