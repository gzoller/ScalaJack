package co.blocke.scalajack
package json

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.reflect.{ReflectOnType, TypeSymbolMapper}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

/** This class is horrible.  It is a mirror of JsonWriter, except this one executes at runtime, and hence
  * doens't have any Expr's.  This is bad becuause it's slow--we're not able to take advantage of doing work
  * at compile-time.  This is ONLY used becuase of trait handling.  The problem is we need to express some
  * class C in terms of trait T.  We know T at compile-time but don't know C until compile-time.  There's
  * (so far) no good way to get the information passed across the bridge, so...  What we need to do in
  * JsonWriter is leverage RType.inTermsOf to do the C->T magic, but this returns an RType, not an
  * RTypeRef.  We must render the rest of the trait (and any structures below it) at runtime, necessitating
  * this class, JsonWriterRT.
  *
  * It should only be used for this special trait handling.  Everything else leverages the compile-time
  * JsonWriter.
  */

object JsonWriterRT:

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  def isOkToWrite(a: Any, cfg: JsonConfig) =
    a match
      case None if !cfg.noneAsNull                                    => false
      case Left(None) if !cfg.noneAsNull                              => false
      case Right(None) if !cfg.noneAsNull                             => false
      case Failure(_) if cfg.tryFailureHandling == TryOption.NO_WRITE => false
      case _                                                          => true

  def refWriteRT[T](
      cfg: JsonConfig,
      rt: RType[T],
      a: T,
      sb: StringBuilder,
      isMapKey: Boolean = false
  )(using classesSeen: scala.collection.mutable.Map[TypedName, RType[?]]): StringBuilder =
    rt match
      case StringRType() | CharRType() | JavaCharacterRType() => if a == null then sb.append("null") else sb.append("\"" + a.toString + "\"")
      case t: PrimitiveRType =>
        if isMapKey then
          if a == null then sb.append("\"null\"")
          else
            sb.append('"')
            sb.append(a.toString)
            sb.append('"')
        else if a == null then sb.append("null")
        else sb.append(a.toString)

      case t: SeqRType[?] =>
        if isMapKey then throw new JsonError("Seq instances cannot be map keys")
        if a == null then sb.append("null")
        else sb.append('[')
        val sbLen = sb.length
        a.asInstanceOf[Seq[t.elementType.T]].foldLeft(sb) { (acc, one) =>
          if isOkToWrite(one, cfg) then
            refWriteRT[t.elementType.T](cfg, t.elementType.asInstanceOf[RType[t.elementType.T]], one, acc)
            sb.append(',')
          else sb
        }
        if sbLen == sb.length then sb.append(']')
        else sb.setCharAt(sb.length() - 1, ']')

      case t: ArrayRType[?] =>
        if isMapKey then throw new JsonError("Arrays instances cannot be map keys")
        if a == null then sb.append("null")
        else sb.append('[')
        val sbLen = sb.length
        a.asInstanceOf[Seq[t.elementType.T]].foldLeft(sb) { (acc, one) =>
          if isOkToWrite(one, cfg) then
            refWriteRT[t.elementType.T](cfg, t.elementType.asInstanceOf[RType[t.elementType.T]], one, acc)
            sb.append(',')
          else sb
        }
        if sbLen == sb.length then sb.append(']')
        else sb.setCharAt(sb.length() - 1, ']')

      case t: ScalaClassRType[?] =>
        classesSeen.put(t.typedName, t)
        if a == null then sb.append("null")
        else
          sb.append('{')
          val sbLen = sb.length
          t.renderTrait.map(traitName => sb.append(s"\"_hint\":\"$traitName\","))
          t.fields.foldLeft(sb) { (acc, f) =>
            val m = a.getClass.getMethod(f.name)
            m.setAccessible(true)
            val fieldValue = m.invoke(a).asInstanceOf[f.fieldType.T]
            if isOkToWrite(fieldValue, cfg) then
              acc.append('"')
              acc.append(f.name)
              acc.append('"')
              acc.append(':')
              refWriteRT[f.fieldType.T](cfg, f.fieldType.asInstanceOf[RType[f.fieldType.T]], fieldValue, acc)
              acc.append(',')
            else acc
          }
          if sbLen == sb.length then sb.append('}')
          else sb.setCharAt(sb.length() - 1, '}')

      case t: TraitRType[?] =>
        classesSeen.put(t.typedName, t)
        val classRType = RType.inTermsOf[T](a.getClass).asInstanceOf[ScalaClassRType[T]].copy(renderTrait = Some(t.name)).asInstanceOf[RType[T]]
        JsonWriterRT.refWriteRT[classRType.T](cfg, classRType, a.asInstanceOf[classRType.T], sb)

      case t: OptionRType[?] =>
        if isMapKey then throw new JsonError("Option valuess cannot be map keys")
        a match
          case None => sb.append("null")
          case Some(v) =>
            refWriteRT[t.optionParamType.T](cfg, t.optionParamType.asInstanceOf[RType[t.optionParamType.T]], v.asInstanceOf[t.optionParamType.T], sb)

      case t: MapRType[?] =>
        if isMapKey then throw new JsonError("Map values cannot be map keys")
        if a == null then sb.append("null")
        else
          sb.append('{')
          val sbLen = sb.length
          a.asInstanceOf[Map[?, ?]].foreach { case (key, value) =>
            if isOkToWrite(value, cfg) then
              val b = refWriteRT[t.elementType.T](cfg, t.elementType.asInstanceOf[RType[t.elementType.T]], key.asInstanceOf[t.elementType.T], sb, true)
              b.append(':')
              val b2 = refWriteRT[t.elementType2.T](cfg, t.elementType2.asInstanceOf[RType[t.elementType2.T]], value.asInstanceOf[t.elementType2.T], sb)
              b2.append(',')
          }
          if sbLen == sb.length then sb.append('}')
          else sb.setCharAt(sb.length() - 1, '}')

      case t: EitherRType[?] =>
        if isMapKey then throw new JsonError("Union, Intersection, or Either-typed values cannot be map keys.")
        a match
          case Left(v) =>
            refWriteRT[t.leftType.T](cfg, t.leftType.asInstanceOf[RType[t.leftType.T]], v.asInstanceOf[t.leftType.T], sb)
          case Right(v) =>
            refWriteRT[t.rightType.T](cfg, t.rightType.asInstanceOf[RType[t.rightType.T]], v.asInstanceOf[t.rightType.T], sb)

      case t: UnionRType[?] =>
        // Intersection/Union types....take your best shot!  It's all we've got.  No definitive info here.
        val trial = new StringBuilder()
        val lrSb = scala.util.Try(
          refWriteRT[t.rightType.T](cfg, t.rightType.asInstanceOf[RType[t.rightType.T]], a.asInstanceOf[t.rightType.T], trial)
        ) match
          case Success(trialSb) => trialSb
          case Failure(_) =>
            trial.clear
            refWriteRT[t.leftType.T](cfg, t.leftType.asInstanceOf[RType[t.leftType.T]], a.asInstanceOf[t.leftType.T], trial)
        sb ++= lrSb

      case t: IntersectionRType[?] =>
        // Intersection/Union types....take your best shot!  It's all we've got.  No definitive info here.
        val trial = new StringBuilder()
        val lrSb = scala.util.Try(
          refWriteRT[t.rightType.T](cfg, t.rightType.asInstanceOf[RType[t.rightType.T]], a.asInstanceOf[t.rightType.T], trial)
        ) match
          case Success(trialSb) => trialSb
          case Failure(_) =>
            trial.clear
            refWriteRT[t.leftType.T](cfg, t.leftType.asInstanceOf[RType[t.leftType.T]], a.asInstanceOf[t.leftType.T], trial)
        sb ++= lrSb

      case t: TryRType[?] =>
        if isMapKey then throw new JsonError("Try values (Succeed/Fail) cannot be map keys")
        a match
          case Success(v) =>
            refWriteRT[t.tryType.T](cfg, t.tryType.asInstanceOf[RType[t.tryType.T]], v.asInstanceOf[t.tryType.T], sb)
          case Failure(_) if cfg.tryFailureHandling == TryOption.AS_NULL => sb.append("null")
          case Failure(f) if cfg.tryFailureHandling == TryOption.THROW_EXCEPTION =>
            throw new JsonError("A try value was Failure with message: " + f.getMessage())
          case Failure(v) =>
            sb.append('"')
            sb.append(v.getMessage)
            sb.append('"')

      case t: TupleRType[?] =>
        if isMapKey then throw new JsonError("Tuples cannot be map keys")
        if a == null then sb.append("null")
        sb.append('[')
        val sbLen = sb.length
        val fieldValues = a.asInstanceOf[Product].productIterator.toList
        t.typeParamValues.zipWithIndex.foreach { case (rt, i) =>
          val fieldValue = fieldValues(i).asInstanceOf[rt.T]
          refWriteRT[rt.T](cfg, rt.asInstanceOf[RType[rt.T]], fieldValue, sb)
          sb.append(',')
        }
        if sbLen == sb.length then sb.append(']')
        else sb.setCharAt(sb.length() - 1, ']')

      case t: JavaCollectionRType[?] =>
        if isMapKey then throw new JsonError("Collections cannot be map keys.")
        if a == null then sb.append("null")
        sb.append('[')
        val sbLen = sb.length
        a.asInstanceOf[java.util.Collection[?]].toArray.foreach { elem =>
          if isOkToWrite(elem, cfg) then refWriteRT[t.elementType.T](cfg, t.elementType.asInstanceOf[RType[t.elementType.T]], elem.asInstanceOf[t.elementType.T], sb)
        }
        sb.append(',')
        if sbLen == sb.length then sb.append(']')
        else sb.setCharAt(sb.length() - 1, ']')

      case t: JavaMapRType[?] =>
        if isMapKey then throw new JsonError("Collections cannot be map keys.")
        if a == null then sb.append("null")
        sb.append('{')
        val sbLen = sb.length
        a.asInstanceOf[java.util.Map[?, ?]].asScala.foreach { case (key, value) =>
          if isOkToWrite(value, cfg) then
            refWriteRT[t.elementType.T](cfg, t.elementType.asInstanceOf[RType[t.elementType.T]], key.asInstanceOf[t.elementType.T], sb, true)
            sb.append(':')
            refWriteRT[t.elementType2.T](cfg, t.elementType2.asInstanceOf[RType[t.elementType2.T]], value.asInstanceOf[t.elementType2.T], sb)
            sb.append(',')
        }
        if sbLen == sb.length then sb.append('}')
        else sb.setCharAt(sb.length() - 1, '}')

      case t: AliasRType[?] =>
        refWriteRT[t.unwrappedType.T](cfg, t.unwrappedType.asInstanceOf[RType[t.unwrappedType.T]], a.asInstanceOf[t.unwrappedType.T], sb)

      case t: SelfRefRType[?] =>
        if isMapKey then throw new JsonError("Classes or traits cannot be map keys.")
        val again = classesSeen.getOrElse(
          t.typedName, {
            // Need to add to cache.  Since we're coming from compile-time side, the runtime side may not have seen this class before...
            val v = RType.of[T]
            classesSeen.put(t.typedName, v)
            v
          }
        )
        JsonWriterRT.refWriteRT[again.T](cfg, again, a.asInstanceOf[again.T], sb)

      case t: EnumRType[?] =>
        val enumAsId = cfg.enumsAsIds match
          case '*'                                           => true
          case aList: List[String] if aList.contains(t.name) => true
          case _                                             => false
        if enumAsId then
          val enumVal = t.ordinal(a.toString).getOrElse(throw new JsonError(s"Value $a is not a valid enum value for ${t.name}"))
          if isMapKey then
            sb.append('"')
            sb.append(enumVal.toString)
            sb.append('"')
          else sb.append(enumVal.toString)
        else
          sb.append('"')
          sb.append(a.toString)
          sb.append('"')

      case t: ObjectRef =>
        sb.append("\"" + t.name + "\"")

      case t: Scala2RType[?] =>
        cfg.undefinedFieldHandling match
          case UndefinedValueOption.AS_NULL         => sb.append("null")
          case UndefinedValueOption.AS_SYMBOL       => sb.append("\"" + t.name + "\"")
          case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + a.toString + " is of some unknown/unsupported Scala 2 type " + t.name)

      case t: UnknownRType[?] =>
        cfg.undefinedFieldHandling match
          case UndefinedValueOption.AS_NULL         => sb.append("null")
          case UndefinedValueOption.AS_SYMBOL       => sb.append("\"" + t.name + "\"")
          case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + a.toString + " is of some unknown/unsupported type " + t.name)

      case t: TypeSymbolRType =>
        cfg.undefinedFieldHandling match
          case UndefinedValueOption.AS_NULL         => sb.append("null")
          case UndefinedValueOption.AS_SYMBOL       => sb.append("\"" + t.name + "\"")
          case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + a.toString + " is of some unknown/unsupported type " + t.name + ". (Class didn't fully define all its type parameters.)")
