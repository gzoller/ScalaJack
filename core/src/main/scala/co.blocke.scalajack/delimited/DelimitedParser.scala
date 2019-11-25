package co.blocke.scalajack
package delimited

import model._
import scala.collection.mutable
import scala.reflect.runtime.universe.Type
import typeadapter.ClassTypeAdapterBase

case class DelimitedParser(
    delimChar:  Char,
    input:      DELIMITED,
    jackFlavor: JackFlavor[DELIMITED])
  extends Parser {
  type WIRE = DELIMITED

  val dChars: Array[Char] = input.toCharArray
  var i = 0
  val max: Int = dChars.length

  @inline def nullCheck(): Boolean =
    dChars(i) == 'n' && i + 4 <= max && input.substring(i, i + 4) == "null"
  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '-' || char == '+'

  def expectString(nullOK: Boolean = true): String = {
    var inQuotes = false
    var done = false
    val acc = new java.lang.StringBuilder()
    while (i < max && !done) {
      dChars(i) match {
        case this.delimChar if !inQuotes =>
          done = true
        case '"' if !inQuotes && i + 1 < max && dChars(i + 1) != '"' =>
          inQuotes = true
          i += 1
        case '"' if inQuotes && (i + 1 == max || dChars(i + 1) != '"') =>
          inQuotes = false
          i += 1
          done = true
        case '"' if i + 1 < max && dChars(i + 1) == '"' =>
          acc.append(dChars(i))
          i += 2
        case _ => // do nothing
          acc.append(dChars(i))
          i += 1
      }
    }
    if (i < max) i += 1 // skip delimiter
    acc.toString
  }

  def expectList[K, TO](
      KtypeAdapter: TypeAdapter[K],
      builder:      mutable.Builder[K, TO]): TO =
    expectString() match {
      case "" => null.asInstanceOf[TO]
      case listStr =>
        val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
        while (subParser.i < subParser.max) builder += KtypeAdapter.read(
          subParser
        )
        builder.result()
    }

  def expectNumber(): String =
    expectString() match {
      case "" => null
      case candidate =>
        candidate.toCharArray.find(c => !isNumberChar(c)) match {
          case None => candidate
          case Some(_) =>
            backspace()
            backspace()
            throw new ScalaJackError(showError("Expected a Number here"))
        }
    }

  def expectBoolean(): Boolean =
    expectString() match {
      case "true"  => true
      case "false" => false
      case _ =>
        backspace()
        backspace()
        throw new ScalaJackError(showError("Expected a Boolean here"))
    }

  def expectTuple(
      readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] =
    expectString() match {
      case "" => null.asInstanceOf[List[Any]]
      case listStr =>
        val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
        readFns.map { fn =>
          fn.valueTypeAdapter.read(subParser)
        }
    }

  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO =
    null.asInstanceOf[TO] // No map support in delimited format

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, String]) = {
    if (classBase.orderedFieldNames.isEmpty)
      throw new ScalaJackError(
        showError(
          "Only case classes with non-empty constructors are supported for delimited data."
        )
      )
    val fieldBits = classBase.fieldBitsTemplate.clone()
    val args = classBase.orderedFieldNames.map { name =>
      val oneField = classBase.fieldMembersByName(name)
      fieldBits -= oneField.index
      oneField.valueTypeAdapter match {
        case ccta: typeadapter.CaseClassTypeAdapter[_] =>
          expectString() match {
            case "" => null
            case listStr =>
              val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
              ccta.read(subParser)
          }
        case ta => ta.read(this)
      }
    }.toArray
    (fieldBits, args, new java.util.HashMap[String, String]())
  }

  def showError(msg: String): String = {
    val (clip, dashes) = i match {
      case ep if ep <= 50 && max < 80 => (input, ep)
      case ep if ep <= 50             => (input.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= max =>
        ("..." + input.substring(i - 49), 52)
      case ep => ("..." + input.substring(ep - 49, ep + 27) + "...", 52)
    }
    msg + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }

  def peekForNull: Boolean =
    if (nullCheck()) {
      i += 4
      true
    } else false

  def resolveTypeMembers(
      typeMembersByName: Map[String, ClassHelper.TypeMember[_]],
      converterFn:       HintBijective
  ): Map[Type, Type] =
    throw new ScalaJackError(
      showError("DelimitedFlavor does not support classes with type members")
    )

  def scanForHint(hint: String, converterFn: HintBijective): Type =
    throw new ScalaJackError(
      showError("DelimitedFlavor does not support traits")
    )

  def skipOverElement(): Unit = {} // has no meaning for delimited input, i.e. no trait or capture support that would require skipping
  def backspace(): Unit = i -= 1
  def mark(): Int = i
  def revertToMark(mark: Int): Unit = i = mark
  def nextIsString: Boolean = true
  def nextIsNumber: Boolean = false
  def nextIsObject: Boolean = false
  def nextIsArray: Boolean = i < max && dChars(i) == '"'
  def nextIsBoolean: Boolean = false
  def subParser(input: DELIMITED): Parser =
    DelimitedParser(delimChar, input, jackFlavor)
  def sourceAsString: String = input
}
