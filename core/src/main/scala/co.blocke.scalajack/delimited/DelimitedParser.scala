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

  private var pos: Int = 0
  private val delimPrefixString = DELIM_PREFIX.toString

  val (tokens, indexes) = {
    val dChars: Array[Char] = input.toCharArray
    var i = 0
    val maxChars: Int = dChars.length
    val tokenList = scala.collection.mutable.ListBuffer.empty[String]
    val indexList = scala.collection.mutable.ListBuffer.empty[Int]
    while (i < maxChars) {
      var inQuotes = false
      var done = false
      val acc = new java.lang.StringBuilder()
      indexList += i
      while (i < maxChars && !done) {
        dChars(i) match {
          case DELIM_PREFIX =>
            acc.append(delimPrefixString)
            done = true
          case this.delimChar if !inQuotes =>
            done = true
          case '"' if !inQuotes && i + 1 < maxChars && dChars(i + 1) != '"' =>
            inQuotes = true
            i += 1
          case '"' if inQuotes && (i + 1 == maxChars || dChars(i + 1) != '"') =>
            inQuotes = false
            i += 1
            done = true
          case '"' if i + 1 < maxChars && dChars(i + 1) == '"' =>
            acc.append(dChars(i))
            i += 2
          case _ => // do nothing
            acc.append(dChars(i))
            i += 1
        }
      }
      tokenList += acc.toString
      if (i < maxChars) i += 1 // skip delimiter
    }
    if (i > 0 && dChars(i - 1) == delimChar) {
      tokenList += ""
      indexList += i
    }
    (tokenList.toList, indexList.toList)
  }

  val max: Int = tokens.size

  @inline def isNumberChar(char: Char): Boolean =
    ('0' <= char && char <= '9') || char == '.' || char == 'e' || char == 'E' || char == '-' || char == '+'

  def expectString(nullOK: Boolean = true): String =
    if (pos < max) {
      val ret = tokens(pos)
      pos += 1
      ret
    } else throw new ScalaJackError(showError("Attempt to read beyond input"))

  def expectList[K, TO](
      KtypeAdapter: TypeAdapter[K],
      builder:      mutable.Builder[K, TO]): TO =
    expectString() match {
      case "" => null.asInstanceOf[TO]
      case listStr =>
        val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
        while (subParser.pos < subParser.max) builder += KtypeAdapter.read(
          subParser
        )
        builder.result()
    }

  def expectNumber(nullsOK: Boolean = false): String =
    expectString() match {
      // $COVERAGE-OFF$Never called--nulls caught in CollectionTypeAdapter before coming here.  Left here as a safety
      case "" if nullsOK => null
      case "" =>
        backspace()
        throw new ScalaJackError(showError("Expected a Number here"))
      // $COVERAGE-ON$
      case candidate =>
        candidate.toCharArray.find(c => !isNumberChar(c)) match {
          case None => candidate
          case Some(_) =>
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
        throw new ScalaJackError(showError("Expected a Boolean here"))
    }

  def expectTuple(
      readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] =
    expectString() match {
      // $COVERAGE-OFF$Can't test--can't validly tell whether "" is escaped '"' or empty value in this context.  Blame CSV
      case "" => null.asInstanceOf[List[Any]]
      // $COVERAGE-ON$
      case listStr =>
        val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
        readFns.map {
          _.valueTypeAdapter match {
            case ccta: Classish =>
              ccta.read(
                DelimitedParser(delimChar, subParser.expectString(), jackFlavor)
              )
            case ta => ta.read(subParser)
          }
        }
    }

  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO =
    throw new ScalaJackError(showError("No Map support for delimited data.")) // No map support in delimited format

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, _]) = {
    if (!classBase.isCaseClass)
      throw new ScalaJackError(
        showError(
          "Only case classes with non-empty constructors are supported for delimited data."
        )
      )
    val fieldBits = classBase.fieldBitsTemplate.clone()

    val args = classBase.argsTemplate.clone()
    classBase.orderedFieldNames.foreach { name =>
      val oneField = classBase.fieldMembersByName(name)
      val valueRead = oneField.valueTypeAdapter match {
        case ccta: typeadapter.CaseClassTypeAdapter[_] =>
          expectString() match {
            case "" => null
            case listStr =>
              val subParser = DelimitedParser(delimChar, listStr, jackFlavor)
              ccta.read(subParser)
          }
        case ta => ta.read(this)
      }
      valueRead match {
        case None                                    =>
        case null if oneField.defaultValue.isDefined =>
        case _ =>
          args(oneField.index) = valueRead
          fieldBits -= oneField.index
      }
    }
    (fieldBits, args, new java.util.HashMap[String, String]())
  }

  def showError(msg: String): String = {
    val inputStr = input.drop(1) // Account for prefix char on input
    val (clip, dashes) = indexes(pos) - 1 match {
      case ep if ep <= 50 && max < 80 => (inputStr, ep)
      case ep if ep <= 50             => (inputStr.substring(0, 77) + "...", ep)
      case ep if ep > 50 && ep + 30 >= max =>
        ("..." + inputStr.substring(indexes(pos) - 50), 52)
      case ep => ("..." + inputStr.substring(ep - 49, ep + 27) + "...", 52)
    }
    msg + "\n" + clip.replaceAll("[\n\t]", "~") + "\n" + ("-" * dashes) + "^"
  }

  def peekForNull: Boolean = {
    val isNull = pos == max || tokens(pos) == "" || (tokens(pos) == delimPrefixString && pos == max - 1)
    if (pos < max && isNull || pos < max && tokens(pos) == delimPrefixString)
      pos += 1
    isNull
  }

  // $COVERAGE-OFF$No meaning for delimited input
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

  def nextIsObject: Boolean = false
  def nextIsArray: Boolean = false
  def nextIsBoolean: Boolean = false
  def sourceAsString: String = input
  // $COVERAGE-ON$

  def backspace(): Unit = pos -= 1
  def mark(): Int = pos
  def revertToMark(mark: Int): Unit = pos = mark
  def nextIsString: Boolean = true
  def nextIsNumber: Boolean = {
    val save = pos
    val isValidNumber = expectString().toCharArray.forall(c => isNumberChar(c))
    pos = save
    isValidNumber
  }
  def subParser(input: DELIMITED): Parser =
    DelimitedParser(delimChar, input, jackFlavor)
}
