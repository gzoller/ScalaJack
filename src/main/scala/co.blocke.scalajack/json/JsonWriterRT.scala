package co.blocke.scalajack
package json

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.reflect.{ReflectOnType, TypeSymbolMapper}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.util.{Failure, Success, Try}

/**
  * This class is horrible.  It is a mirror of JsonWriter, except this one executes at runtime, and hence
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
        else if a == null then sb.append("null") else sb.append(a.toString)

      case t: SeqRType[?] =>
        if isMapKey then throw new JsonError("Seq instances cannot be map keys")
        if a == null then sb.append("null")
        else
        sb.append('[')
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
        else
        sb.append('[')
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
            t.renderTrait.map( traitName => sb.append(s"\"_hint\":\"$traitName\",") )
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

      case t: TryRType[?] =>
        if isMapKey then throw new JsonError("Try values (Succeed/Fail) cannot be map keys")
        a match
          case Success(v) =>
            refWriteRT[t.tryType.T](cfg, t.tryType.asInstanceOf[RType[t.tryType.T]], v.asInstanceOf[t.tryType.T], sb)
          case Failure(_) if cfg.tryFailureHandling == TryOption.AS_NULL => sb.append("null")
          case Failure(v) =>
            sb.append('"')
            sb.append(v.getMessage)
            sb.append('"')

      case t: AliasRType[?] =>
        refWriteRT[t.unwrappedType.T](cfg, t.unwrappedType.asInstanceOf[RType[t.unwrappedType.T]], a.asInstanceOf[t.unwrappedType.T], sb)

      case t: SelfRefRType[?] =>
        if isMapKey then throw new JsonError("Classes or traits cannot be map keys.")
        val again = classesSeen.getOrElse(
          t.typedName, 
          {
            // Need to add to cache.  Since we're coming from compile-time side, the runtime side may not have seen this class before...
            val v = RType.of[T]
            classesSeen.put(t.typedName, v)
            v
          }
          )
        JsonWriterRT.refWriteRT[again.T](cfg, again, a.asInstanceOf[again.T], sb)
