package co.blocke.scalajack
package json

import scala.quoted.*
import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.{EnumRType, ScalaClassRType, TraitRType}
import co.blocke.scala_reflection.reflect.{ReflectOnType, TypeSymbolMapper}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.util.{Failure, Success, Try}
import scala.quoted.staging.*

/*
  TODO:
    [ ] - Scala non-case class
    [ ] - Java class (Do I still want to support this???)
    [*] - Enum
    [*] - Enumeration
    [*] - Java Enum
    [ ] - Java Collections
    [ ] - Java Map
    [*] - Intersection
    [*] - Union
    [*] - Either
    [*] - Object (How???)
    [ ] - Sealed Trait (How???)
    [*] - SelfRef
    [ ] - Tuple
    [*] - Unknown (throw exception)
    [*] - Scala 2 (throw exception)
    [*] - TypeSymbol (throw exception)
 */

object JsonWriter:

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  def isOkToWrite(a: Any, cfg: JsonConfig) =
    a match
      case None if !cfg.noneAsNull                                    => false
      case Left(None) if !cfg.noneAsNull                              => false
      case Right(None) if !cfg.noneAsNull                             => false
      case Failure(_) if cfg.tryFailureHandling == TryOption.NO_WRITE => false
      case _                                                          => true

  def refWrite[T](
      cfgE: Expr[JsonConfig],
      ref: RTypeRef[T],
      aE: Expr[T],
      sbE: Expr[StringBuilder],
      isMapKey: Boolean = false
  )(using classesSeen: scala.collection.mutable.Map[TypedName, RTypeRef[?]])(using Quotes, Type[T]): Expr[StringBuilder] =
    import quotes.reflect.*

    ref match
      case t: PrimitiveRef[?] if t.family == PrimFamily.Stringish => '{ if $aE == null then $sbE.append("null") else $sbE.append("\"" + $aE.toString + "\"") }
      case t: PrimitiveRef[?] =>
        if isMapKey then
          '{
            if $aE == null then $sbE.append("\"null\"")
            else
              $sbE.append('"')
              $sbE.append($aE.toString)
              $sbE.append('"')
          }
        else '{ if $aE == null then $sbE.append("null") else $sbE.append($aE.toString) }

      case t: SeqRef[?] =>
        if isMapKey then throw new JsonError("Seq instances cannot be map keys")

        t.elementRef.refType match
          case '[e] =>
            '{
              val sb = $sbE
              if $aE == null then sb.append("null")
              else
                sb.append('[')
                val sbLen = sb.length

                $aE.asInstanceOf[Seq[e]].foldLeft(sb) { (acc, one) =>
                  if isOkToWrite(one, $cfgE) then
                    ${ refWrite[e](cfgE, t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, '{ acc }) }
                    sb.append(',')
                  else sb
                }

                if sbLen == sb.length then sb.append(']')
                else sb.setCharAt(sb.length() - 1, ']')
            }
      case t: ArrayRef[?] =>
        if isMapKey then throw new JsonError("Arrays cannot be map keys")

        t.elementRef.refType match
          case '[e] =>
            '{
              val sb = $sbE
              if $aE == null then sb.append("null")
              else
                sb.append('[')
                val sbLen = sb.length

                $aE.asInstanceOf[Array[e]].foldLeft(sb) { (acc, one) =>
                  if isOkToWrite(one, $cfgE) then
                    ${ refWrite[e](cfgE, t.elementRef.asInstanceOf[RTypeRef[e]], '{ one }, '{ acc }) }
                    sb.append(',')
                  else sb
                }

                if sbLen == sb.length then sb.append(']')
                else sb.setCharAt(sb.length() - 1, ']')
            }

      case t: ClassRef[?] =>
        classesSeen.put(t.typedName, t)
        '{
          val sb = $sbE
          if $aE == null then sb.append("null")
          else
            sb.append('{')
            val sbLen = sb.length
            ${
              t.fields.foldLeft('{ sb }) { (accE, f) =>
                f.fieldRef.refType match
                  case '[e] =>
                    val fieldValue = Select.unique(aE.asTerm, f.name).asExprOf[e]
                    val name = Expr(f.name)
                    '{
                      val acc = $accE
                      if isOkToWrite($fieldValue, $cfgE) then
                        acc.append('"')
                        acc.append($name)
                        acc.append('"')
                        acc.append(':')
                        ${ refWrite[e](cfgE, f.fieldRef.asInstanceOf[RTypeRef[e]], fieldValue, '{ acc }) }
                        acc.append(',')
                      else acc
                    }
              }
            }
            if sbLen == sb.length then sb.append('}')
            else sb.setCharAt(sb.length() - 1, '}')
        }

      case t: TraitRef[?] =>
        classesSeen.put(t.typedName, t)
        val rt = t.expr
        '{
          given Compiler = Compiler.make($aE.getClass.getClassLoader)
          val fn = (q: Quotes) ?=> {
            import q.reflect.*
            val sb = $sbE
            val classRType = RType.inTermsOf[T]($aE.getClass).asInstanceOf[ScalaClassRType[T]].copy(renderTrait = Some($rt.name)).asInstanceOf[RType[T]]
            JsonWriterRT.refWriteRT[classRType.T]($cfgE, classRType, $aE.asInstanceOf[classRType.T], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
            Expr(1) // do-nothing... '{} requires Expr(something) be returned, so...
          }
          quoted.staging.run(fn)
          $sbE
        }

      case t: OptionRef[?] =>
        if isMapKey then throw new JsonError("Option valuess cannot be map keys")
        t.optionParamType.refType match
          case '[e] =>
            '{
              $aE match
                case None => $sbE.append("null")
                case Some(v) =>
                  ${ refWrite[e](cfgE, t.optionParamType.asInstanceOf[RTypeRef[e]], '{ v }.asInstanceOf[Expr[e]], sbE) }
            }

      case t: MapRef[?] =>
        if isMapKey then throw new JsonError("Map values cannot be map keys")
        t.elementRef.refType match
          case '[k] =>
            t.elementRef2.refType match
              case '[v] =>
                '{
                  val sb = $sbE
                  if $aE == null then sb.append("null")
                  else
                    sb.append('{')
                    val sbLen = sb.length
                    $aE.asInstanceOf[Map[?, ?]].foreach { case (key, value) =>
                      if isOkToWrite(value, $cfgE) then
                        val b = ${ refWrite[k](cfgE, t.elementRef.asInstanceOf[RTypeRef[k]], '{ key }.asInstanceOf[Expr[k]], sbE, true) }
                        b.append(':')
                        val b2 = ${ refWrite[v](cfgE, t.elementRef2.asInstanceOf[RTypeRef[v]], '{ value }.asInstanceOf[Expr[v]], sbE) }
                        b2.append(',')
                    }
                    if sbLen == sb.length then sb.append('}')
                    else sb.setCharAt(sb.length() - 1, '}')
                }

      case t: LeftRightRef[?] =>
        if isMapKey then throw new JsonError("Union, Intersection, or Either-typed values cannot be map keys.")
        t.leftRef.refType match
          case '[lt] =>
            t.rightRef.refType match
              case '[rt] =>
                val isEither = Expr(t.lrkind == LRKind.EITHER)
                '{
                  if $isEither then
                    $aE match
                      case Left(v) =>
                        val vv = v.asInstanceOf[lt]
                        ${ refWrite[lt](cfgE, t.leftRef.asInstanceOf[RTypeRef[lt]], '{ vv }.asInstanceOf[Expr[lt]], sbE) }
                      case Right(v) =>
                        val vv = v.asInstanceOf[rt]
                        ${ refWrite[rt](cfgE, t.rightRef.asInstanceOf[RTypeRef[rt]], '{ vv }.asInstanceOf[Expr[rt]], sbE) }
                  else
                    // Intersection/Union types....take your best shot!  It's all we've got.  No definitive info here.
                    val trial = new StringBuilder()
                    val lrSb = scala.util.Try {
                      val vv = $aE.asInstanceOf[rt]
                      ${
                        refWrite[rt](cfgE, t.rightRef.asInstanceOf[RTypeRef[rt]], '{ vv }, '{ trial })
                      }
                    } match
                      case Success(trialSb) => trialSb
                      case Failure(_) =>
                        trial.clear
                        val vv = $aE.asInstanceOf[lt]
                        ${
                          refWrite[lt](cfgE, t.leftRef.asInstanceOf[RTypeRef[lt]], '{ vv }, '{ trial })
                        }
                    $sbE ++= lrSb
                }

      case t: TryRef[?] =>
        if isMapKey then throw new JsonError("Try values (Succeed/Fail) cannot be map keys")
        t.tryRef.refType match
          case '[e] =>
            '{
              $aE match
                case Success(v) =>
                  ${ refWrite[e](cfgE, t.tryRef.asInstanceOf[RTypeRef[e]], '{ v }.asInstanceOf[Expr[e]], sbE) }
                case Failure(_) if $cfgE.tryFailureHandling == TryOption.AS_NULL => $sbE.append("null")
                case Failure(_) if $cfgE.tryFailureHandling == TryOption.AS_NULL => $sbE.append("null")
                case Failure(f) if $cfgE.tryFailureHandling == TryOption.THROW_EXCEPTION =>
                  throw new JsonError("A try value was Failure with message: " + f.getMessage())
                case Failure(v) =>
                  $sbE.append("\"Failure(")
                  $sbE.append(v.getMessage)
                  $sbE.append(")\"")
            }

      case t: AliasRef[?] =>
        t.unwrappedType.refType match
          case '[e] =>
            refWrite[e](cfgE, t.unwrappedType.asInstanceOf[RTypeRef[e]], aE.asInstanceOf[Expr[e]], sbE)

      case t: SelfRefRef[?] =>
        if isMapKey then throw new JsonError("Classes or traits cannot be map keys.")
        import quotes.reflect.*
        val againE = classesSeen.getOrElse(t.typedName, throw new JsonError("Dangling self-reference: " + t.name)).asInstanceOf[RTypeRef[T]].expr
        '{
          val again = $againE.asInstanceOf[RType[T]]
          JsonWriterRT.refWriteRT[T]($cfgE, again, $aE.asInstanceOf[T], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
          $sbE
        }

      case t: EnumRef[?] =>
        val enumE = t.expr
        val isMapKeyE = Expr(isMapKey)
        '{
          val enumRT = $enumE.asInstanceOf[EnumRType[T]]
          val enumAsId = $cfgE.enumsAsIds match
            case '*'                                                => true
            case aList: List[String] if aList.contains(enumRT.name) => true
            case _                                                  => false
          if enumAsId then
            val enumVal = enumRT.ordinal($aE.toString).getOrElse(throw new JsonError("Value " + $aE.toString + s" is not a valid enum value for ${enumRT.name}"))
            if $isMapKeyE then
              $sbE.append('"')
              $sbE.append(enumVal.toString)
              $sbE.append('"')
            else $sbE.append(enumVal.toString)
          else
            $sbE.append('"')
            $sbE.append($aE.toString)
            $sbE.append('"')
        }

      case t: ObjectRef =>
        val tname = Expr(t.name)
        '{
          $sbE.append("\"" + $tname + "\"")
        }

      case t: Scala2Ref[?] =>
        val tname = Expr(t.name)
        '{
          $cfgE.undefinedFieldHandling match
            case UndefinedValueOption.AS_NULL         => $sbE.append("null")
            case UndefinedValueOption.AS_SYMBOL       => $sbE.append("\"" + $tname + "\"")
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + $aE.toString + " is of some unknown/unsupported Scala 2 type " + $tname)
        }

      case t: UnknownRef[?] =>
        val tname = Expr(t.name)
        '{
          $cfgE.undefinedFieldHandling match
            case UndefinedValueOption.AS_NULL         => $sbE.append("null")
            case UndefinedValueOption.AS_SYMBOL       => $sbE.append("\"" + $tname + "\"")
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + $aE.toString + " is of some unknown/unsupported type " + $tname)
        }

      case t: TypeSymbolRef =>
        val tname = Expr(t.name)
        '{
          $cfgE.undefinedFieldHandling match
            case UndefinedValueOption.AS_NULL         => $sbE.append("null")
            case UndefinedValueOption.AS_SYMBOL       => $sbE.append("\"" + $tname + "\"")
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Value " + $aE.toString + " is of some unknown/unsupported type " + $tname + ". (Class didn't fully define all its type parameters.)")
        }
