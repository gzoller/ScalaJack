package co.blocke.scalajack
package json4s

import org.json4s._
import model._
import typeadapter.ClassTypeAdapterBase
import scala.collection.mutable
import scala.reflect.runtime.universe.Type

case class Json4sParser(input: JValue, jackFlavor: JackFlavor[JValue])
  extends Parser {
  type WIRE = JValue

  def expectString(nullOK: Boolean = true): String =
    input match {
      case null | JNull => null
      case JString(s)   => s
      case x =>
        throw new ScalaJackError(s"Expected string here, not '$x'")
    }

  def expectList[K, TO](
      KtypeAdapter: TypeAdapter[K],
      builder:      mutable.Builder[K, TO]): TO =
    input match {
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      case null | JNull => null.asInstanceOf[TO]
      // $COVERAGE-ON$
      case JArray(arr) =>
        arr.foreach(
          a => builder += KtypeAdapter.read(subParser(a)).asInstanceOf[K]
        )
        builder.result()
      case x =>
        throw new ScalaJackError(s"Expected list here, not '$x'")
    }

  def expectTuple(
      readFns: List[typeadapter.TupleTypeAdapterFactory.TupleField[_]]
  ): List[Any] =
    input match {
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      case null | JNull => null
      // $COVERAGE-ON$
      case JArray(arr) =>
        val together = readFns.zip(arr)
        together.map {
          case (fn, v) => fn.valueTypeAdapter.read(subParser(v))
        }
      case x =>
        throw new ScalaJackError(s"Expected tuple (list) here, not '$x'")
    }

  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO =
    input match {
      // $COVERAGE-OFF$Null caught by TypeAdapter but this left here as a safety
      case null | JNull => null.asInstanceOf[TO]
      // $COVERAGE-ON$
      case JObject(obj) =>
        obj.foreach {
          case (key, objVal) =>
            val mapKey = keyTypeAdapter.read(subParser(JString(key)))
            val mapValue = valueTypeAdapter.read(subParser(objVal))
            val newElem: (K, V) = (mapKey, mapValue)
            builder += newElem
        }
        builder.result
      case x =>
        throw new ScalaJackError(s"Expected map here, not '$x'")
    }

  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, Array[Any], java.util.HashMap[String, _]) =
    input match {
      case JObject(obj) =>
        val args = classBase.argsTemplate.clone()
        val fieldBits = classBase.fieldBitsTemplate.clone()
        val captured =
          if (classBase.isSJCapture) new java.util.HashMap[String, JValue]()
          else null
        obj.foreach {
          case (key, objVal) =>
            classBase.fieldMembersByName
              .get(key)
              .map { field =>
                fieldBits -= field.index
                args(field.index) =
                  field.valueTypeAdapter.read(subParser(objVal))
              }
              .getOrElse {
                if (captured != null)
                  captured.put(key, objVal)
              }
        }
        (fieldBits, args, captured)
      case x =>
        throw new ScalaJackError(s"Expected object here, not '$x'")
    }

  def expectBoolean(): Boolean =
    input match {
      case JBool(b) => b
      case x =>
        throw new ScalaJackError(s"Expected boolean here, not '$x'")
    }

  def expectNumber(): String =
    input match {
      case null | JNull  => null
      case JDecimal(num) => num.toString
      case JDouble(num)  => num.toString
      case JInt(num)     => num.toString
      case JLong(num)    => num.toString
      case _             => throw new ScalaJackError(s"Expected number here, not '$input'")
    }

  def peekForNull: Boolean = input match {
    case null | JNull => true
    case _            => false
  }

  def scanForHint(hint: String, converterFn: HintBijective): Type =
    input match {
      case JObject(obj) =>
        obj
          .find(_._1 == hint)
          .map {
            case (label, hintValue) =>
              hintValue match {
                case s: JString =>
                  try {
                    converterFn.apply(s.s)
                  } catch {
                    case t: Throwable =>
                      throw new ScalaJackError(
                        s"Couldn't marshal class for ${s.s}"
                      )
                  }
                case _ =>
                  throw new ScalaJackError(
                    s"Hint value $hint must be a string value"
                  )
              }
          }
          .getOrElse(throw new ScalaJackError(s"Type hint '$hint' not found"))
      case x =>
        throw new ScalaJackError(s"Expected object here, not '$x'")
    }

  // For embedded type members.  Convert the type member into runtime "actual" type, e.g. T --> Foo
  def resolveTypeMembers(
      typeMembersByName: Map[String, ClassHelper.TypeMember[_]],
      converterFn:       HintBijective
  ): Map[Type, Type] = // Returns Map[Type Signature Type (e.g. 'T'), Type]
    input match {
      case JObject(obj) =>
        val collected = obj.collect {
          case (key, oneValue) if typeMembersByName.contains(key) && oneValue
            .isInstanceOf[JString] =>
            (
              typeMembersByName(key).typeSignature,
              converterFn.apply(oneValue.asInstanceOf[JString].s)
            )
        }
        collected.toMap
      case x =>
        throw new ScalaJackError(s"Expected object here, not '$x'")
    }

  // $COVERAGE-OFF$Not needed for Json4s
  def showError(msg: String): String = msg
  def skipOverElement(): Unit = {}
  def backspace(): Unit = {}
  def mark(): Int = 0
  def revertToMark(mark: Int): Unit = {}
  // $COVERAGE-ON$

  def nextIsString: Boolean = input.isInstanceOf[JString]
  def nextIsNumber: Boolean =
    input match {
      case JDecimal(_) => true
      case JDouble(_)  => true
      case JInt(_)     => true
      case JLong(_)    => true
      case _           => false
    }
  def nextIsObject: Boolean = input.isInstanceOf[JObject]
  def nextIsArray: Boolean = input.isInstanceOf[JArray]
  def nextIsBoolean: Boolean = input.isInstanceOf[JBool]

  def subParser(input: JValue): Parser = Json4sParser(input, jackFlavor)
  def sourceAsString: String = input.toString
}
