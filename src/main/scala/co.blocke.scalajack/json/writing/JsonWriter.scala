package co.blocke.scalajack
package json

import scala.quoted.*
import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.{EnumRType, NonConstructorFieldInfo, ScalaClassRType, TraitRType}
import co.blocke.scala_reflection.reflect.{ReflectOnType, TypeSymbolMapper}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import scala.quoted.staging.*

/*
  TODO:
    [*] - Scala non-case class
    [*] - Java class (Do I still want to support this???)
    [*] - Enum
    [*] - Enumeration
    [*] - Java Enum
    [*] - Java Collections
    [*] - Java Map
    [*] - Intersection
    [*] - Union
    [*] - Either
    [*] - Object (How???)
    [*] - Trait (How???)
    [*] - Sealed trait
    [*] - sealed abstract class (handle like sealed trait....)
    [*] - SelfRef
    [*] - Tuple
    [*] - Unknown (throw exception)
    [*] - Scala 2 (throw exception)
    [*] - TypeSymbol (throw exception)
    [*] - Value class

    [*] -- correct all the 'if $aE == null...'
    [*] -- type hint label mapping
    [*] -- type hint value mapping
    [*] -- Discontinue use of inTermsOf "open" (non-sealed) trait support (?!)
    [*] -- update runtime-size TraitRType handling to match new compile-time code

    [ ] -- Streaming JSON write support
    [ ] -- BigJSON support (eg. multi-gig file)
 */

object JsonWriter:

  final inline def lastPart(n: String) = n.split('.').last.stripSuffix("$")

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  def isOkToWrite(a: Any, cfg: JsonConfig) =
    a match
      case None if !cfg.noneAsNull                                    => false
      case o: java.util.Optional[?] if o.isEmpty && !cfg.noneAsNull   => false
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
        val isNullable = Expr(t.isNullable)
        if isMapKey then
          '{
            if $isNullable && $aE == null then $sbE.append("\"null\"")
            else
              $sbE.append('"')
              $sbE.append($aE.toString)
              $sbE.append('"')
          }
        else '{ if $isNullable && $aE == null then $sbE.append("null") else $sbE.append($aE.toString) }

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

      case t: ScalaClassRef[?] if t.isSealed && t.isAbstractClass =>
        classesSeen.put(t.typedName, t)
        if t.childrenAreObject then
          // case object -> just write the simple name of the object
          '{
            if $aE == null then $sbE.append("null")
            else
              $sbE.append('"')
              $sbE.append(lastPart($aE.getClass.getName))
              $sbE.append('"')
          }
        else
          // Wow!  If this is a sealed trait, all the children already have correctly-typed parameters--no need for expensive inTermsOf()
          val rt = t.expr.asInstanceOf[Expr[ScalaClassRType[T]]]
          '{
            if $aE == null then $sbE.append("null")
            else
              val className = $aE.getClass.getName
              $rt.sealedChildren
                .find(_.name == className)
                .map(foundKid =>
                  val augmented = foundKid.asInstanceOf[ScalaClassRType[foundKid.T]].copy(renderTrait = Some($rt.name)).asInstanceOf[RType[foundKid.T]]
                  JsonWriterRT.refWriteRT[foundKid.T]($cfgE, augmented, $aE.asInstanceOf[foundKid.T], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
                )
                .getOrElse(throw new JsonError(s"Unrecognized child $className of seald trait " + $rt.name))
          }

      case t: ScalaClassRef[?] if t.isValueClass =>
        val theField = t.fields.head.fieldRef
        theField.refType match
          case '[e] =>
            val fieldValue = Select.unique(aE.asTerm, t.fields.head.name).asExprOf[e]
            refWrite[e](cfgE, theField.asInstanceOf[RTypeRef[e]], fieldValue, sbE)

      case t: ScalaClassRef[?] =>
        if t.isAbstractClass then throw new JsonError("Cannot serialize an abstract class")
        classesSeen.put(t.typedName, t)
        val isCase = Expr(t.isCaseClass)
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
            if ! $isCase && $cfgE.writeNonConstructorFields then
              ${
                t.nonConstructorFields.foldLeft('{ sb }) { (accE, f) =>
                  f.fieldRef.refType match
                    case '[e] =>
                      val fieldValue = Select.unique(aE.asTerm, f.getterLabel).asExprOf[e]
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
        val rt = t.expr.asInstanceOf[Expr[TraitRType[T]]]
        if t.childrenAreObject then
          // case object -> just write the simple name of the object
          '{
            if $aE == null then $sbE.append("null")
            else
              $sbE.append('"')
              $sbE.append(lastPart($aE.getClass.getName))
              $sbE.append('"')
          }
        else if t.isSealed then
          // Wow!  If this is a sealed trait, all the children already have correctly-typed parameters--no need for expensive inTermsOf()
          '{
            if $aE == null then $sbE.append("null")
            else
              val className = $aE.getClass.getName
              $rt.sealedChildren
                .find(_.name == className)
                .map(foundKid =>
                  val augmented = foundKid.asInstanceOf[ScalaClassRType[foundKid.T]].copy(renderTrait = Some($rt.name)).asInstanceOf[RType[foundKid.T]]
                  JsonWriterRT.refWriteRT[foundKid.T]($cfgE, augmented, $aE.asInstanceOf[foundKid.T], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
                )
                .getOrElse(throw new JsonError(s"Unrecognized child $className of seald trait " + $rt.name))
          }
        else throw new JsonError("non-sealed traits are not supported")
      // '{
      //   if $aE == null then $sbE.append("null")
      //   else
      //     given Compiler = Compiler.make($aE.getClass.getClassLoader)
      //     val fn = (q: Quotes) ?=> {
      //       import q.reflect.*
      //       val sb = $sbE
      //       val classRType = RType.inTermsOf[T]($aE.getClass).asInstanceOf[ScalaClassRType[T]].copy(renderTrait = Some($rt.name)).asInstanceOf[RType[T]]
      //       JsonWriterRT.refWriteRT[classRType.T]($cfgE, classRType, $aE.asInstanceOf[classRType.T], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
      //       Expr(1) // do-nothing... '{} requires Expr(something) be returned, so...
      //     }
      //     quoted.staging.run(fn)
      //     $sbE
      // }

      case t: OptionRef[?] =>
        if isMapKey then throw new JsonError("Option valuess cannot be map keys")
        t.optionParamType.refType match
          case '[e] =>
            '{
              if $aE == null then $sbE.append("null")
              else
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
              if $aE == null then $sbE.append("null")
              else
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

      case t: TupleRef[?] =>
        if isMapKey then throw new JsonError("Tuples cannot be map keys")
        '{
          val sb = $sbE
          if $aE == null then sb.append("null")
          else
            sb.append('[')
            val sbLen = sb.length
            ${
              val tupleBuf = t.tupleRefs.zipWithIndex.foldLeft(sbE) { case (accE, (ref, i)) =>
                ref.refType match
                  case '[e] =>
                    val fieldValue = Select.unique(aE.asTerm, "_" + (i + 1)).asExprOf[e]
                    '{
                      val acc = $accE
                      ${ refWrite[e](cfgE, ref.asInstanceOf[RTypeRef[e]], fieldValue, '{ acc }) }
                      acc.append(',')
                    }
              }
              tupleBuf
            }
            if sbLen == sb.length then sb.append(']')
            else sb.setCharAt(sb.length() - 1, ']')
        }

      case t: JavaCollectionRef[?] =>
        if isMapKey then throw new JsonError("Collections cannot be map keys.")
        t.elementRef.refType match
          case '[e] =>
            '{
              val sb = $sbE
              if $aE == null then sb.append("null")
              sb.append('[')
              val sbLen = sb.length
              $aE.asInstanceOf[java.util.Collection[?]].toArray.foreach { elem =>
                if isOkToWrite(elem, $cfgE) then
                  ${ refWrite[e](cfgE, t.elementRef.asInstanceOf[RTypeRef[e]], '{ elem }.asInstanceOf[Expr[e]], sbE) }
                  sb.append(',')
              }
              if sbLen == sb.length then sb.append(']')
              else sb.setCharAt(sb.length() - 1, ']')
            }

      case t: JavaMapRef[?] =>
        if isMapKey then throw new JsonError("Maps cannot be map keys.")
        t.elementRef.refType match
          case '[k] =>
            t.elementRef2.refType match
              case '[v] =>
                '{
                  val sb = $sbE
                  if $aE == null then sb.append("null")
                  sb.append('{')
                  val sbLen = sb.length
                  $aE.asInstanceOf[java.util.Map[?, ?]].asScala.foreach { case (key, value) =>
                    if isOkToWrite(value, $cfgE) then
                      ${ refWrite[k](cfgE, t.elementRef.asInstanceOf[RTypeRef[k]], '{ key }.asInstanceOf[Expr[k]], sbE, true) }
                      sb.append(':')
                      ${ refWrite[v](cfgE, t.elementRef2.asInstanceOf[RTypeRef[v]], '{ value }.asInstanceOf[Expr[v]], sbE) }
                      sb.append(',')
                  }
                  if sbLen == sb.length then sb.append('}')
                  else sb.setCharAt(sb.length() - 1, '}')
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
          if $aE == null then $sbE.append("null")
          else
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

      // Just handle Java classes 100% runtime since we need to leverage Java reflection entirely anyway
      case t: JavaClassRef[?] =>
        t.refType match
          case '[e] =>
            '{
              JsonWriterRT.refWriteRT[e]($cfgE, ${ t.expr }.asInstanceOf[RType[e]], $aE.asInstanceOf[e], $sbE)(using scala.collection.mutable.Map.empty[TypedName, RType[?]])
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
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Unknown/unsupported Scala 2 type " + $tname)
        }

      case t: UnknownRef[?] =>
        val tname = Expr(t.name)
        '{
          $cfgE.undefinedFieldHandling match
            case UndefinedValueOption.AS_NULL         => $sbE.append("null")
            case UndefinedValueOption.AS_SYMBOL       => $sbE.append("\"" + $tname + "\"")
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Unknown/unsupported type " + $tname)
        }

      case t: TypeSymbolRef =>
        val tname = Expr(t.name)
        '{
          $cfgE.undefinedFieldHandling match
            case UndefinedValueOption.AS_NULL         => $sbE.append("null")
            case UndefinedValueOption.AS_SYMBOL       => $sbE.append("\"" + $tname + "\"")
            case UndefinedValueOption.THROW_EXCEPTION => throw new JsonError("Unknown/unsupported type " + $tname + ". (Class didn't fully define all its type parameters.)")
        }
