package co.blocke.scalajack
package json

import model._
import ClassHelper.ExtraFieldValue

import scala.collection.{ GenIterable, GenMap }
import scala.collection.immutable.ListMap
import scala.collection.mutable.Builder

trait JsonWriter extends Writer[String] {

  this: JsonTransciever =>

  @inline def addString(s: String, out: Builder[Any, String]): Unit = s.toCharArray.map(c => out += c)

  def writeArray[Elem](t: GenIterable[Elem], elemTypeAdapter: TypeAdapter[Elem], out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += '['
      val iter = a.iterator
      while (iter.hasNext) {
        elemTypeAdapter.write(iter.next, this, out)
        if (iter.hasNext)
          out += ','
      }
      out += ']'
  }

  def writeBigInt(t: BigInt, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case s    => addString(s.toString, out)
  }

  def writeBoolean(t: Boolean, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeDecimal(t: BigDecimal, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case s    => addString(s.toString, out)
  }

  def writeDouble(t: Double, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeInt(t: Int, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeLong(t: Long, out: Builder[Any, String]): Unit =
    addString(t.toString, out)

  def writeMap[Key, Value, To](t: GenMap[Key, Value], keyTypeAdapter: TypeAdapter[Key], valueTypeAdapter: TypeAdapter[Value], out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case a =>
      out += '{'
      val iter = a.iterator
      while (iter.hasNext) {
        val kv = iter.next
        keyTypeAdapter.write(kv._1, this, out)
        out += ':'
        valueTypeAdapter.write(kv._2, this, out)
        if (iter.hasNext)
          out += ','
      }
      out += '}'
  }

  def writeRawString(t: String, out: Builder[Any, String]): Unit = t match {
    case null      => addString("null", out)
    case s: String => addString(s, out)
  }

  def writeString(t: String, out: Builder[Any, String]): Unit = t match {
    case null => addString("null", out)
    case _: String =>
      out += '"'
      var i = 0
      val length = t.length
      val chars = t.toCharArray

      while (i < length) {
        chars(i) match {
          case '"'  => addString("""\"""", out)
          case ' '  => addString(" ", out)
          case '\\' => addString("""\\""", out)
          case '/'  => addString("""\/""", out)
          case '\b' => addString("""\b""", out)
          case '\f' => addString("""\f""", out)
          case '\n' => addString("""\n""", out)
          case '\r' => addString("""\r""", out)
          case '\t' => addString("""\t""", out)
          case ch if ch <= 32 || ch >= 128 =>
            addString("""\""" + "u" + "%04x".format(ch.toInt), out)
          case c => out += c
        }

        i += 1
      }
      out += '"'
  }

  def writeNull(out: Builder[Any, String]): Unit = addString("null", out)

  def writeObject[T](
    t:            T,
    fieldMembers: ListMap[String, ClassHelper.ClassFieldMember[T, Any]],
    out:          Builder[Any, String],
    extras:       List[(String, ExtraFieldValue[_])]): Unit = {
    if (t == null) {
      addString("null", out)
    } else {
      out += '{'

      /*
      if (typeMembers.nonEmpty) {
        import scala.collection.mutable

        val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

        for (fieldMember <- fieldMembers) {
          val fieldValue = fieldMember.valueIn(value)
          val declaredFieldValueType = fieldMember.declaredValueType
          val actualFieldValueType = Reflection.inferTypeOf(fieldValue)(fieldMember.valueTypeTag)

          for (typeParam <- tpe.typeConstructor.typeParams) {
            for (typeMember <- typeMembers) {
              val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
                haystackBeforeSubstitution = declaredFieldValueType,
                haystackAfterSubstitution  = actualFieldValueType,
                needleBeforeSubstitution   = typeParam.asType.toType)

              for (typeArg <- optionalTypeArg) {
                setsOfTypeArgsByTypeParam.getOrElseUpdate(typeParam, new mutable.HashSet[Type]) += typeArg
              }
            }
          }
        }

        val substitutions: List[(Symbol, Type)] = (for ((typeParam, setOfTypes) <- setsOfTypeArgsByTypeParam) yield {
          typeParam -> universe.lub(setOfTypes.toList)
        }).toList

        val substitutionMap = substitutions.toMap

        val typeParams = tpe.typeConstructor.typeParams
        val typeArgs = typeParams.map(typeParam => substitutionMap(typeParam))

        for (typeMember <- typeMembers) {
          val ttt = typeMember.typeSignature.substituteTypes(substitutions.map(_._1), substitutions.map(_._2))
          memberNameTypeAdapter.write(typeMember.name, writer)
          typeTypeAdapter.write(ttt, writer)
        }

        val newType = appliedType(tpe.typeConstructor, typeArgs)
        val newTypeAdapter = context.typeAdapter(newType).asInstanceOf[ClassLikeTypeAdapter[T]]

        for (member <- newTypeAdapter.fieldMembers) {
          val memberValue = member.valueIn(value)

          memberNameTypeAdapter.write(member.name, writer)
          member.writeValue(memberValue, writer)
        }
      } else {
      */
      var first = true
      for ((label, extraVal) <- extras) {
        if (first)
          first = false
        else
          out += ','
        writeString(label, out)
        out += ':'
        extraVal.write(this, out)
      }
      for ((memberName, member) <- fieldMembers) {
        if (first)
          first = false
        else
          out += ','
        //        val memberName = mappedFieldsByName.get(member.name).map(_.fieldMapName.get).getOrElse(member.name)
        writeString(memberName, out)
        out += ':'
        val memberValue = member.valueIn(t)
        member.valueTypeAdapter.write(memberValue, this, out)
      }

      /*
      value match {
        case sjc: SJCapture =>
          sjc.captured.foreach {
            case (memberName, valueString) =>
              memberNameTypeAdapter.write(memberName, writer)
              writer.writeRawValue(valueString.asInstanceOf[String])
          }
        case _ =>
      }
      */

      out += '}'
    }
  }
}
