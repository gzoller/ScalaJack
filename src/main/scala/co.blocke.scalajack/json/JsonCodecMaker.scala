package co.blocke.scalajack
package json

import writing.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.{EnumRType, JavaClassRType, NonConstructorFieldInfo}
import reading.JsonSource
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.reflect.ClassTag
import scala.annotation.{switch, tailrec}
import scala.collection.Factory
import scala.util.{Failure, Success, Try}
import dotty.tools.dotc.ast.Trees.EmptyTree
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.lang3.text.translate.CharSequenceTranslator
import dotty.tools.dotc.core.TypeComparer.AnyConstantType

object JsonCodecMaker:

  def generateCodecFor[T](ref: RTypeRef[T], cfg: JsonConfig)(using q: Quotes)(using tt: Type[T]) =
    import q.reflect.*

    // Cache generated method Symbols + an array of the generated functions (DefDef)
    case class MethodKey(ref: RTypeRef[?], isStringified: Boolean) // <-- TODO: Not clear what isStringified does here...

    val writeMethodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val writeMethodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]

    // Fantastic Dark Magic here--lifted from Jasoniter.  Props!  This thing will create a DefDef, and a Symbol to it.
    // The Symbol will let you call the generated function later from other macro-generated code.  The goal is to use
    // generated functions to create cleaner/faster macro code than what straight quotes/splices would create unaided.
    def makeWriteFn[U: Type](methodKey: MethodKey, arg: Expr[U], out: Expr[JsonOutput])(f: (Expr[U], Expr[JsonOutput]) => Expr[Unit]): Expr[Unit] =
      // Get a symbol, if one already created for this key... else make one.
      Apply(
        Ref(
          writeMethodSyms.getOrElse(
            methodKey, {
              val sym = Symbol.newMethod(
                Symbol.spliceOwner,
                "w" + writeMethodSyms.size, // 'w' is for Writer!
                MethodType(List("in", "out"))(_ => List(TypeRepr.of[U], TypeRepr.of[JsonOutput]), _ => TypeRepr.of[Unit])
              )
              writeMethodSyms.update(methodKey, sym)
              writeMethodDefs += DefDef(
                sym,
                params => {
                  val List(List(in, out)) = params
                  Some(f(in.asExprOf[U], out.asExprOf[JsonOutput]).asTerm.changeOwner(sym))
                }
              )
              sym
            }
          )
        ),
        List(arg.asTerm, out.asTerm)
      ).asExprOf[Unit]

    val readMethodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val readMethodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]

    def makeReadFn[U: Type](methodKey: MethodKey, in: Expr[JsonSource])(f: Expr[JsonSource] => Expr[U])(using Quotes)(using Type[JsonSource]): Expr[Unit] =
      readMethodSyms.getOrElse(
        methodKey, {
          val sym = Symbol.newMethod(
            Symbol.spliceOwner,
            "r" + readMethodSyms.size,
            MethodType(List("in"))(_ => List(TypeRepr.of[JsonSource]), _ => TypeRepr.of[U])
            //                    (_ => List(input_params,...), _ => resultType)
          )
          readMethodSyms.update(methodKey, sym)
          readMethodDefs += DefDef(
            sym,
            params => {
              val List(List(in)) = params
              Some(f(in.asExprOf[JsonSource]).asTerm.changeOwner(sym))
            }
          )
        }
      )
      '{}

    val classFieldMatrixSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val classFieldMatrixValDefs = new scala.collection.mutable.ArrayBuffer[ValDef]

    def makeClassFieldMatrixValDef(methodKey: MethodKey, className: String, fieldNames: Array[String])(using Quotes): Expr[Unit] =
      classFieldMatrixSyms.getOrElse(
        methodKey, {
          val sym = Symbol.newVal(
            Symbol.spliceOwner,
            s"__$className" + "_fields",
            TypeRepr.of[StringMatrix],
            Flags.EmptyFlags,
            Symbol.noSymbol
          )
          classFieldMatrixSyms.update(methodKey, sym)
          val names = Expr(fieldNames)
          classFieldMatrixValDefs += ValDef(sym, Some('{ new StringMatrix($names) }.asTerm))
        }
      )
      '{}

    // ---------------------------------------------------------------------------------------------

    def testValidMapKey(testRef: RTypeRef[?]): Boolean =
      val isValid = testRef match
        case _: PrimitiveRef => true
        case _: TimeRef      => true
        case _: NetRef       => true
        case a: AliasRef[?]  => testValidMapKey(a.unwrappedType)
        case _               => false
      if !isValid then throw new JsonTypeError(s"For JSON serialization, map keys must be a simple type. ${testRef.name} is too complex.")
      isValid

    def maybeWrite[T](label: String, aE: Expr[T], ref: RTypeRef[T], out: Expr[JsonOutput], cfg: JsonConfig): Expr[Unit] =
      val labelE = Expr(label)
      _maybeWrite[T](
        '{ $out.label($labelE) },
        aE,
        ref,
        out,
        cfg
      )

    def maybeWriteMap[K, V](keyE: Expr[K], valueE: Expr[V], keyRef: RTypeRef[K], valueRef: RTypeRef[V], out: Expr[JsonOutput], cfg: JsonConfig): Expr[Unit] =
      keyRef.refType match
        case '[k] =>
          _maybeWrite[V](
            '{
              $out.maybeComma()
              ${ genWriteVal(keyE.asExprOf[k], keyRef.asInstanceOf[RTypeRef[k]], out, false, true) }
              $out.colon()
            },
            valueE,
            valueRef,
            out,
            cfg
          )

    // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
    // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
    // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
    // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
    // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
    def _maybeWrite[T](prefix: Expr[Unit], aE: Expr[T], ref: RTypeRef[T], out: Expr[JsonOutput], cfg: JsonConfig): Expr[Unit] =
      ref match
        case t: ScalaOptionRef[?] if !cfg.noneAsNull =>
          t.optionParamType.refType match
            case '[e] =>
              val tin = aE.asExprOf[Option[e]]
              '{
                $tin match
                  case None    => ()
                  case Some(v) => ${ _maybeWrite[e](prefix, '{ v }.asExprOf[e], t.optionParamType.asInstanceOf[RTypeRef[e]], out, cfg) }
              }
        case t: JavaOptionalRef[?] if !cfg.noneAsNull =>
          t.optionParamType.refType match
            case '[e] =>
              val tin = aE.asExprOf[java.util.Optional[e]]
              '{
                if ! $tin.isEmpty then ${ _maybeWrite[e](prefix, '{ $tin.get }.asExprOf[e], t.optionParamType.asInstanceOf[RTypeRef[e]], out, cfg) }
              }
        case t: TryRef[?] =>
          t.tryRef.refType match
            case '[e] =>
              val tin = aE.asExprOf[scala.util.Try[e]]
              '{
                $tin match
                  case scala.util.Failure(v) =>
                    ${
                      cfg.tryFailureHandling match
                        case TryPolicy.AS_NULL =>
                          '{
                            $prefix
                            $out.burpNull()
                          }
                        case TryPolicy.ERR_MSG_STRING =>
                          '{
                            $prefix
                            $out.value("Try Failure with msg: " + v.getMessage())
                          }
                        case TryPolicy.THROW_EXCEPTION => '{ throw v }
                    }
                  case _ => ${ _maybeWrite[e](prefix, '{ $tin.get }.asExprOf[e], t.tryRef.asInstanceOf[RTypeRef[e]], out, cfg) }
              }
        case t: LeftRightRef[?] if t.lrkind == LRKind.EITHER =>
          t.refType match
            case '[u] =>
              val tin = aE.asExprOf[u]
              t.rightRef.refType match
                case '[rt] =>
                  cfg.eitherLeftHandling match
                    case EitherLeftPolicy.AS_NULL =>
                      '{
                        if $tin == null then $out.burpNull()
                        $tin match
                          case Left(_) =>
                            $prefix
                            $out.burpNull()
                          case Right(v) => ${ _maybeWrite[rt](prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, cfg) }
                      }
                    case EitherLeftPolicy.ERR_MSG_STRING =>
                      '{
                        if $tin == null then $out.burpNull()
                        $tin match
                          case Left(err) =>
                            $prefix
                            $out.value("Left Error: " + err.toString)
                          case Right(v) => ${ _maybeWrite[rt](prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, cfg) }
                      }
                    case EitherLeftPolicy.THROW_EXCEPTION =>
                      '{
                        if $tin == null then $out.burpNull()
                        $tin match
                          case Left(err) => throw new JsonEitherLeftError("Left Error: " + err.toString)
                          case Right(v)  => ${ _maybeWrite[rt](prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, cfg) }
                      }
                    case EitherLeftPolicy.AS_VALUE =>
                      t.leftRef.refType match
                        case '[lt] =>
                          '{
                            if $tin == null then $out.burpNull()
                            $tin match
                              case Left(v) =>
                                ${ _maybeWrite[lt](prefix, '{ v.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, cfg) }
                              case Right(v) =>
                                ${ _maybeWrite[rt](prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, cfg) }
                          }
        case t: LeftRightRef[?] if t.lrkind != LRKind.EITHER =>
          t.refType match
            case '[e] =>
              t.rightRef.refType match
                case '[rt] =>
                  t.leftRef.refType match
                    case '[lt] =>
                      val tin = aE.asExprOf[e]
                      '{
                        if $tin == null then $out.burpNull()
                        else
                          $out.mark()
                          scala.util.Try {
                            ${ _maybeWrite[rt](prefix, '{ $tin.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, cfg) }
                          } match
                            case scala.util.Success(_) => ()
                            case scala.util.Failure(f) =>
                              $out.revert()
                              ${ _maybeWrite[lt](prefix, '{ $tin.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, cfg) }
                      }
        case t: AnyRef =>
          AnyWriter.okToWrite2(prefix, aE, out, cfg)
        case _ =>
          ref.refType match
            case '[u] =>
              '{
                $prefix
                ${ genWriteVal[u](aE.asExprOf[u], ref.asInstanceOf[RTypeRef[u]], out) }
              }

    // ---------------------------------------------------------------------------------------------

    def genEncFnBody[T](r: RTypeRef[?], aE: Expr[T], out: Expr[JsonOutput], emitDiscriminator: Boolean = false, inTuple: Boolean = false)(using Quotes): Expr[Unit] =
      r.refType match
        case '[b] =>
          r match
            case t: ArrayRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = in.asInstanceOf[Expr[Array[e]]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                        $out.endArray()
                    }
              }

            case t: SeqRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Seq[e]] else in.asExprOf[Seq[e]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                        $out.endArray()
                    }
              }

            case t: SetRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Set[e]] else in.asExprOf[Set[e]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal('{ i }.asExprOf[e], t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                        $out.endArray()
                    }
              }

            case t: ScalaClassRef[?] if t.isSealed && t.isAbstractClass => // basically just like sealed trait...
              if t.childrenAreObject then
                val tin = aE.asExprOf[b]
                // case object -> just write the simple name of the object
                '{
                  $out.value($tin.getClass.getName.split('.').last.stripSuffix("$"))
                }
              else
                val cases = t.sealedChildren.map { child =>
                  child.refType match
                    case '[c] =>
                      val subtype = TypeIdent(TypeRepr.of[c].typeSymbol)
                      val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, subtype.tpe)
                      CaseDef(Bind(sym, Typed(Ref(sym), subtype)), None, genEncFnBody[c](child, Ref(sym).asExprOf[c], out, true).asTerm)
                } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                val matchExpr = Match(aE.asTerm, cases).asExprOf[Unit]
                matchExpr

            // We don't use makeWriteFn here because a value class is basically just a "box" around a simple type
            case t: ScalaClassRef[?] if t.isValueClass =>
              val theField = t.fields.head.fieldRef
              theField.refType match
                case '[e] =>
                  val fieldValue = Select.unique(aE.asTerm, t.fields.head.name).asExprOf[e]
                  genWriteVal(fieldValue, theField.asInstanceOf[RTypeRef[e]], out)

            case t: ScalaClassRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                val body = {
                  val eachField = t.fields.map { f =>
                    f.fieldRef.refType match
                      case '[z] =>
                        val fieldValue = Select.unique(in.asTerm, f.name).asExprOf[z]
                        val fieldName = f.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(f.name)
                        maybeWrite[z](fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out, cfg)
                  }
                  if emitDiscriminator then
                    val cname = cfg.typeHintPolicy match
                      case TypeHintPolicy.SIMPLE_CLASSNAME   => Expr(lastPart(t.name))
                      case TypeHintPolicy.SCRAMBLE_CLASSNAME => '{ scramble(${ Expr(lastPart(t.name).hashCode) }) }
                      case TypeHintPolicy.USE_ANNOTATION =>
                        Expr(t.annotations.get("co.blocke.scalajack.TypeHint").flatMap(_.get("hintValue")).getOrElse(lastPart(t.name)))
                    val withDisc = '{
                      $out.label(${ Expr(cfg.typeHintLabel) })
                      $out.value($cname)
                    } +: eachField
                    Expr.block(withDisc.init, withDisc.last)
                  else if eachField.length == 1 then eachField.head
                  else Expr.block(eachField.init, eachField.last)
                }

                if !t.isCaseClass && cfg.writeNonConstructorFields then
                  val eachField = t.nonConstructorFields.map { f =>
                    f.fieldRef.refType match
                      case '[e] =>
                        val fieldValue = Select.unique(in.asTerm, f.getterLabel).asExprOf[e]
                        val fieldName = f.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(f.name)
                        maybeWrite[e](fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[e]], out, cfg)
                  }
                  val subBody = eachField.length match
                    case 0 => '{}
                    case 1 => eachField.head
                    case _ => Expr.block(eachField.init, eachField.last)
                  '{
                    if $in == null then $out.burpNull()
                    else
                      $out.startObject()
                      $body
                      $subBody
                      $out.endObject()
                  }
                else
                  '{
                    if $in == null then $out.burpNull()
                    else
                      $out.startObject()
                      $body
                      $out.endObject()
                  }
              }

            case t: MapRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[k] =>
                    t.elementRef2.refType match
                      case '[v] =>
                        val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Map[k, v]] else in.asExprOf[Map[k, v]]
                        '{
                          if $tin == null then $out.burpNull()
                          else
                            $out.startObject()
                            $tin.foreach { case (key, value) =>
                              ${
                                (t.elementRef, t.elementRef2) match
                                  case (aliasK: AliasRef[?], aliasV: AliasRef[?]) =>
                                    aliasK.unwrappedType.refType match
                                      case '[ak] =>
                                        aliasV.unwrappedType.refType match
                                          case '[av] =>
                                            testValidMapKey(aliasK.unwrappedType)
                                            maybeWriteMap[ak, av](
                                              '{ key.asInstanceOf[ak] },
                                              '{ value.asInstanceOf[av] },
                                              aliasK.unwrappedType.asInstanceOf[RTypeRef[ak]],
                                              aliasV.unwrappedType.asInstanceOf[RTypeRef[av]],
                                              out,
                                              cfg
                                            )
                                  case (_, aliasV: AliasRef[?]) =>
                                    aliasV.unwrappedType.refType match
                                      case '[av] =>
                                        testValidMapKey(t.elementRef)
                                        maybeWriteMap[k, av](
                                          '{ key }.asExprOf[k],
                                          '{ value.asInstanceOf[av] },
                                          t.elementRef.asInstanceOf[RTypeRef[k]],
                                          aliasV.unwrappedType.asInstanceOf[RTypeRef[av]],
                                          out,
                                          cfg
                                        )
                                  case (aliasK: AliasRef[?], _) =>
                                    aliasK.unwrappedType.refType match
                                      case '[ak] =>
                                        testValidMapKey(aliasK.unwrappedType)
                                        maybeWriteMap[ak, v](
                                          '{ key.asInstanceOf[ak] },
                                          '{ value }.asExprOf[v],
                                          aliasK.unwrappedType.asInstanceOf[RTypeRef[ak]],
                                          t.elementRef2.asInstanceOf[RTypeRef[v]],
                                          out,
                                          cfg
                                        )
                                  case (_, _) =>
                                    testValidMapKey(t.elementRef)
                                    maybeWriteMap[k, v]('{ key }, '{ value }.asExprOf[v], t.elementRef.asInstanceOf[RTypeRef[k]], t.elementRef2.asInstanceOf[RTypeRef[v]], out, cfg)
                              }
                            }
                            $out.endObject()
                        }
              }

            case t: JavaCollectionRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = in.asExprOf[java.util.Collection[_]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.toArray.foreach { elem =>
                          ${ genWriteVal('{ elem.asInstanceOf[e] }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                        $out.endArray()
                    }
              }

            case t: JavaMapRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[k] =>
                    t.elementRef2.refType match
                      case '[v] =>
                        val tin = in.asExprOf[java.util.Map[k, v]]
                        '{
                          if $tin == null then $out.burpNull()
                          else
                            $out.startObject()
                            $tin.asScala.foreach { case (key, value) =>
                              ${
                                maybeWriteMap[k, v]('{ key }, '{ value }.asExprOf[v], t.elementRef.asInstanceOf[RTypeRef[k]], t.elementRef2.asInstanceOf[RTypeRef[v]], out, cfg)
                              }
                            }
                            $out.endObject()
                        }
              }

            case t: JavaClassRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.refType match
                  case '[p] =>
                    val rtype = t.expr.asExprOf[JavaClassRType[p]]
                    val tin = in.asExprOf[b]
                    var fieldRefs = t.fields.asInstanceOf[List[NonConstructorFieldInfoRef]]
                    val sref = ReflectOnType[String](q)(TypeRepr.of[String])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startObject()
                        val rt = $rtype
                        rt.fields.foreach { f =>
                          val field = f.asInstanceOf[NonConstructorFieldInfo]
                          val m = $tin.getClass.getMethod(field.getterLabel)
                          m.setAccessible(true)
                          val fieldValue = m.invoke($tin)
                          val fieldName = field.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(f.name)
                          ${
                            val ref = fieldRefs.head
                            fieldRefs = fieldRefs.tail
                            ref.fieldRef.refType match
                              case '[e] =>
                                maybeWriteMap[String, e]('{ fieldName }, '{ fieldValue.asInstanceOf[e] }, sref, ref.fieldRef.asInstanceOf[RTypeRef[e]], out, cfg)
                          }
                        }
                        $out.endObject()
                    }
              }

            case t: TraitRef[?] =>
              if !t.isSealed then throw new JsonUnsupportedType("Non-sealed traits are not supported")
              if t.childrenAreObject then
                // case object -> just write the simple name of the object
                val tin = aE.asExprOf[b]
                '{
                  $out.value($tin.getClass.getName.split('.').last.stripSuffix("$"))
                }
              else
                // So... sealed trait children could be any of those defined for the trait.  We need to
                // generate a match/case statement that in turn generates render functions for each
                // child of the sealed trait.
                // Refer to Jsoniter: JsonCodecMaker.scala around line 920 for example how to do this, incl a wildcard.
                // (f: (Expr[U], Expr[JsonOutput]) => Expr[Unit])
                makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                  val cases = t.sealedChildren.map { child =>
                    child.refType match
                      case '[c] =>
                        val subtype = TypeIdent(TypeRepr.of[c].typeSymbol)
                        val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, subtype.tpe)
                        CaseDef(Bind(sym, Typed(Wildcard(), Inferred(subtype.tpe))), None, genEncFnBody[c](child, Ref(sym).asExprOf[c], out, cfg._suppressTypeHints).asTerm)
                  } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                  Match(in.asTerm, cases).asExprOf[Unit]
                }

            // No makeWriteFn here--Option is just a wrapper to the real thingy
            case t: OptionRef[?] =>
              t.optionParamType.refType match
                case '[e] =>
                  val tin = aE.asExprOf[b]
                  '{
                    if $tin == null then $out.burpNull()
                    else
                      $tin match
                        case None =>
                          ${
                            if cfg.noneAsNull || inTuple then '{ $out.burpNull() }
                            else '{ () }
                          }
                        case Some(v) =>
                          val vv = v.asInstanceOf[e]
                          ${ genWriteVal[e]('{ vv }, t.optionParamType.asInstanceOf[RTypeRef[e]], out) }
                  }

            // No makeWriteFn here... SelfRef is referring to something we've already seen before.  There absolutely should already be a geneated
            // and cached function for this thing that we can call.
            case t: SelfRefRef[?] =>
              t.refType match
                case '[e] =>
                  val ref = ReflectOnType[e](q)(TypeRepr.of[e])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                  val key = MethodKey(ref, false)
                  val sym = writeMethodSyms(key)
                  val tin = aE.asExprOf[b]
                  '{
                    if $tin == null then $out.burpNull()
                    else ${ Ref(sym).appliedTo(tin.asTerm, out.asTerm).asExprOf[Unit] }
                  }

            // No makeWriteFn here.  All LeftRight types (Either, Union, Intersection) are just type wrappers
            case t: LeftRightRef[?] =>
              val tin = aE.asExprOf[b]
              t.leftRef.refType match
                case '[lt] =>
                  t.rightRef.refType match
                    case '[rt] =>
                      // This is a close parallel with maybeWrite handling of Either.  If the Either is a field in a class or
                      // Map, the maybeWrite logic applies--because we need to not write both the Either value AND the field label.
                      // If the Either is part of a tuple, Seq, etc., then this logic applies.
                      if t.lrkind == LRKind.EITHER then
                        cfg.eitherLeftHandling match
                          case EitherLeftPolicy.AS_VALUE =>
                            '{
                              if $tin == null then $out.burpNull()
                              else
                                $tin match
                                  case Left(v) =>
                                    ${ genWriteVal[lt]('{ v.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, inTuple = inTuple) }
                                  case Right(v) =>
                                    ${ genWriteVal[rt]('{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                            }
                          case EitherLeftPolicy.AS_NULL =>
                            '{
                              if $tin == null then $out.burpNull()
                              else
                                $tin match
                                  case Left(v) => $out.burpNull()
                                  case Right(v) =>
                                    ${ genWriteVal[rt]('{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                            }
                          case EitherLeftPolicy.ERR_MSG_STRING =>
                            '{
                              if $tin == null then $out.burpNull()
                              else
                                $tin match
                                  case Left(v) => $out.value("Left Error: " + v.toString)
                                  case Right(v) =>
                                    ${ genWriteVal[rt]('{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                            }
                          case EitherLeftPolicy.THROW_EXCEPTION =>
                            '{
                              if $tin == null then $out.burpNull()
                              else
                                $tin match
                                  case Left(v) => throw new JsonEitherLeftError("Left Error: " + v.toString)
                                  case Right(v) =>
                                    ${ genWriteVal[rt]('{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                            }
                      else
                        '{
                          $out.mark()
                          scala.util.Try {
                            ${ genWriteVal[rt]('{ $tin.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                          } match
                            case scala.util.Success(_) => () // do nothing further--write to out already happened
                            case scala.util.Failure(_) =>
                              $out.revert()
                              ${ genWriteVal[lt]('{ $tin.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, inTuple = inTuple) }
                        }

            // No makeWriteFn here.  Try is just a wrapper
            case t: TryRef[?] =>
              t.tryRef.refType match
                case '[e] =>
                  val tin = aE.asExprOf[scala.util.Try[e]]
                  '{
                    if $tin == null then $out.burpNull()
                    else
                      $tin match
                        case scala.util.Success(v) =>
                          ${ genWriteVal[e]('{ v }.asInstanceOf[Expr[e]], t.tryRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        case scala.util.Failure(v) =>
                          ${
                            cfg.tryFailureHandling match
                              case _ if inTuple              => '{ $out.burpNull() }
                              case TryPolicy.AS_NULL         => '{ $out.burpNull() }
                              case TryPolicy.ERR_MSG_STRING  => '{ $out.value("Try Failure with msg: " + v.getMessage()) }
                              case TryPolicy.THROW_EXCEPTION => '{ throw v }
                          }
                  }

            case t: TupleRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                '{
                  if $in == null then $out.burpNull()
                  else
                    $out.startArray()
                    ${
                      // Note: Don't use maybeWrite here... Tuples are fixed-length.  We need to write
                      // something for every position, so write null for None or other "bad" values
                      val elementsE = t.tupleRefs.zipWithIndex.map { case (ref, i) =>
                        ref.refType match
                          case '[e] =>
                            val fieldValue = Select.unique(in.asTerm, "_" + (i + 1)).asExprOf[e]
                            genWriteVal[e](fieldValue, ref.asInstanceOf[RTypeRef[e]], out, inTuple = true)
                      }
                      if elementsE.size == 1 then elementsE.head
                      else Expr.block(elementsE.init, elementsE.last)
                    }
                    $out.endArray()
                }
              }

            case t => throw new JsonUnsupportedType("Type represented by " + t.name + " is unsupported for JSON writes")

    // ---------------------------------------------------------------------------------------------

    def genWriteVal[T: Type](
        aE: Expr[T],
        ref: RTypeRef[T],
        out: Expr[JsonOutput],
        inTuple: Boolean = false,
        isMapKey: Boolean = false // only primitive or primitive-equiv types can be Map keys
    )(using Quotes): Expr[Unit] =
      val methodKey = MethodKey(ref, false)
      writeMethodSyms
        .get(methodKey)
        .map { sym => // hit cache first... then match on Ref type
          Apply(Ref(sym), List(aE.asTerm, out.asTerm)).asExprOf[Unit]
        }
        .getOrElse(
          ref match
            // First cover all primitive and simple types...
            case t: BigDecimalRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[scala.math.BigDecimal] }) }
              else '{ $out.value(${ aE.asExprOf[scala.math.BigDecimal] }) }
            case t: BigIntRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[scala.math.BigInt] }) }
              else '{ $out.value(${ aE.asExprOf[scala.math.BigInt] }) }
            case t: BooleanRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Boolean] }) }
              else '{ $out.value(${ aE.asExprOf[Boolean] }) }
            case t: ByteRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Byte] }) }
              else '{ $out.value(${ aE.asExprOf[Byte] }) }
            case t: CharRef =>
              '{ $out.value(${ aE.asExprOf[Char] }) }
            case t: DoubleRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Double] }) }
              else '{ $out.value(${ aE.asExprOf[Double] }) }
            case t: FloatRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Float] }) }
              else '{ $out.value(${ aE.asExprOf[Float] }) }
            case t: IntRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Int] }) }
              else '{ $out.value(${ aE.asExprOf[Int] }) }
            case t: LongRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Long] }) }
              else '{ $out.value(${ aE.asExprOf[Long] }) }
            case t: ShortRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[Short] }) }
              else '{ $out.value(${ aE.asExprOf[Short] }) }
            case t: StringRef => '{ $out.valueEscaped(${ aE.asExprOf[String] }) }

            case t: JBigDecimalRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.math.BigDecimal] }) }
              else '{ $out.value(${ aE.asExprOf[java.math.BigDecimal] }) }
            case t: JBigIntegerRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.math.BigInteger] }) }
              else '{ $out.value(${ aE.asExprOf[java.math.BigInteger] }) }
            case t: JBooleanRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Boolean] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Boolean] }) }
            case t: JByteRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Byte] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Byte] }) }
            case t: JCharacterRef =>
              '{ $out.value(${ aE.asExprOf[java.lang.Character] }) }
            case t: JDoubleRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Double] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Double] }) }
            case t: JFloatRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Float] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Float] }) }
            case t: JIntegerRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Integer] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Integer] }) }
            case t: JLongRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Long] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Long] }) }
            case t: JShortRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Short] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Short] }) }
            case t: JNumberRef =>
              if isMapKey then '{ $out.valueStringified(${ aE.asExprOf[java.lang.Number] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Number] }) }

            case t: DurationRef       => '{ $out.value(${ aE.asExprOf[java.time.Duration] }) }
            case t: InstantRef        => '{ $out.value(${ aE.asExprOf[java.time.Instant] }) }
            case t: LocalDateRef      => '{ $out.value(${ aE.asExprOf[java.time.LocalDate] }) }
            case t: LocalDateTimeRef  => '{ $out.value(${ aE.asExprOf[java.time.LocalDateTime] }) }
            case t: LocalTimeRef      => '{ $out.value(${ aE.asExprOf[java.time.LocalTime] }) }
            case t: MonthDayRef       => '{ $out.value(${ aE.asExprOf[java.time.MonthDay] }) }
            case t: OffsetDateTimeRef => '{ $out.value(${ aE.asExprOf[java.time.OffsetDateTime] }) }
            case t: OffsetTimeRef     => '{ $out.value(${ aE.asExprOf[java.time.OffsetTime] }) }
            case t: PeriodRef         => '{ $out.value(${ aE.asExprOf[java.time.Period] }) }
            case t: YearRef           => '{ $out.value(${ aE.asExprOf[java.time.Year] }) }
            case t: YearMonthRef      => '{ $out.value(${ aE.asExprOf[java.time.YearMonth] }) }
            case t: ZonedDateTimeRef  => '{ $out.value(${ aE.asExprOf[java.time.ZonedDateTime] }) }
            case t: ZoneIdRef         => '{ $out.value(${ aE.asExprOf[java.time.ZoneId] }) }
            case t: ZoneOffsetRef     => '{ $out.value(${ aE.asExprOf[java.time.ZoneOffset] }) }

            case t: URLRef    => '{ $out.value(${ aE.asExprOf[java.net.URL] }) }
            case t: URIRef    => '{ $out.value(${ aE.asExprOf[java.net.URI] }) }
            case t: UUIDRef   => '{ $out.value(${ aE.asExprOf[java.util.UUID] }) }
            case t: ObjectRef => '{ $out.value(${ Expr(t.name) }) }

            case t: AliasRef[?] =>
              // Special check for RawJson pseudo-type
              if lastPart(t.definedType) == "RawJson" then '{ $out.valueRaw(${ aE.asExprOf[RawJson] }) }
              else
                t.unwrappedType.refType match
                  case '[e] =>
                    genWriteVal[e](aE.asInstanceOf[Expr[e]], t.unwrappedType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple)

            // These one's here becaue Enums and their various flavors can be Map keys
            // (EnumRef handles: Scala 3 enum, Scala 2 Enumeration, Java Enumeration)
            case t: EnumRef[?] =>
              val enumAsId = cfg.enumsAsIds match
                case None                                => false
                case Some(Nil)                           => true
                case Some(list) if list.contains(t.name) => true
                case _                                   => false
              val rtype = t.expr
              if enumAsId then
                if isMapKey then '{ $out.value($rtype.asInstanceOf[EnumRType[_]].ordinal($aE.toString).get.toString) } // stringified id
                else '{ $out.value($rtype.asInstanceOf[EnumRType[_]].ordinal($aE.toString).get) } // int value of id
              else '{ $out.value($aE.toString) }

            // NeoType is a bit of a puzzle-box.  To get the correct underlying base type, I had to dig into
            // the argument of method validate$retainedBody.  It happened to have the correctly-typed parameter.
            // With the correct type, we can correct write out the value.
            case t: NeoTypeRef[?] => // in Quotes context
              Symbol.requiredModule(t.typedName.toString).methodMember("validate$retainedBody").head.paramSymss.head.head.tree match
                case ValDef(_, tt, _) =>
                  tt.tpe.asType match
                    case '[u] =>
                      val baseTypeRef = ReflectOnType.apply(q)(tt.tpe)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                      genWriteVal[u]('{ $aE.asInstanceOf[u] }, baseTypeRef.asInstanceOf[RTypeRef[u]], out)

            /* This is how you call make(), which includes validate()
              val myMake = module.methodMember("make").head
              val tm = Ref(module)
              val z = Apply(
                Select.unique(tm, "make"),
                List(
                  Expr(List(1, 2, 3)).asTerm
                )
              ).asExprOf[Either[String, _]]
              '{
                println("Hello...")
                println($z)
              }
             */

            case t: AnyRef => '{ AnyWriter.writeAny($aE, $out, ${ Expr(cfg) }) }

            // Everything else...
            // case _ if isStringified => throw new JsonIllegalKeyType("Non-primitive/non-simple types cannot be map keys")
            case _ => genEncFnBody(ref, aE, out, inTuple = inTuple)
        )

    // ---------------------------------------------------------------------------------------------

    def lrHasOptionChild(lr: LeftRightRef[?]): String =
      lr.rightRef match
        case t: OptionRef[?]    => "r"
        case t: LeftRightRef[?] => "r" + lrHasOptionChild(t)
        case _ =>
          lr.leftRef match
            case t: OptionRef[?]    => "l"
            case t: LeftRightRef[?] => "l" + lrHasOptionChild(t)
            case _                  => ""

    def genDecFnBody[T: Type](r: RTypeRef[?], in: Expr[JsonSource])(using Quotes): Expr[Unit] =
      import quotes.reflect.*

      def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
        case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
        case _                        => Nil

      r.refType match // refType is Type[r.R]
        case '[b] =>
          r match
            case t: ScalaClassRef[?] =>
              makeReadFn[T](MethodKey(t, false), in)(in =>
                val fieldNames = Expr(t.fields.map(_.name).toArray)

                // Generate vars for each contractor argument, populated with either a "unit" value (eg 0, "") or given default value
                val tpe = TypeRepr.of[b]
                val classCompanion = tpe.typeSymbol.companionClass
                val companionModule = tpe.typeSymbol.companionModule
                val totalRequired = math.pow(2, t.fields.length).toInt - 1
                var required = 0
                val reqSym = Symbol.newVal(Symbol.spliceOwner, "required", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
                val allFieldNames = Expr(t.fields.map(_.name).toArray) // Used for missing required field error

                val together = t.fields.map { oneField =>
                  oneField.fieldRef.refType match {
                    case '[f] =>
                      val dvMembers = classCompanion.methodMember("$lessinit$greater$default$" + (oneField.index + 1))
                      val sym = Symbol.newVal(Symbol.spliceOwner, "_" + oneField.name, TypeRepr.of[f], Flags.Mutable, Symbol.noSymbol)
                      val fieldSymRef = Ident(sym.termRef)
                      val reqBit = Expr(math.pow(2, oneField.index).toInt)
                      val fieldName = Expr(oneField.name)

                      val caseDef = CaseDef(
                        Literal(IntConstant(oneField.index)),
                        None,
                        '{
                          if (${ Ref(reqSym).asExprOf[Int] } & $reqBit) != 0 then
                            ${ Assign(Ref(reqSym), '{ ${ Ref(reqSym).asExprOf[Int] } ^ $reqBit }.asTerm).asExprOf[Unit] }
                            ${ Assign(fieldSymRef, genReadVal[f](oneField.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit] }
                          else throw new JsonParseError("Duplicate field " + $fieldName, $in)
                        }.asTerm
                      )
                      // if dvMembers.isEmpty then (ValDef(sym, Some(oneField.fieldRef.unitVal.asTerm)), caseDef, fieldSymRef)
                      if dvMembers.isEmpty then
                        // no default... required?  Not if Option/Optional, or a collection
                        val unitVal = oneField.fieldRef match {
                          case _: OptionRef[?] =>
                            oneField.fieldRef.unitVal.asTerm // not required
                          case r: LeftRightRef[?] if r.lrkind == LRKind.EITHER => // maybe required
                            val optionRecipe = lrHasOptionChild(r)
                            if optionRecipe.length == 0 then
                              required = required | math.pow(2, oneField.index).toInt // required
                              oneField.fieldRef.unitVal.asTerm
                            else
                              val recipeE = Expr(optionRecipe)
                              '{
                                $recipeE.foldRight(None: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f]
                              }.asTerm
                          case r: LeftRightRef[?] => // maybe required
                            val optionRecipe = lrHasOptionChild(r)
                            if optionRecipe.length == 0 then // no Option children -> required
                              required = required | math.pow(2, oneField.index).toInt // required
                              oneField.fieldRef.unitVal.asTerm
                            else // at least one Option child -> optional
                              '{ None }.asTerm
                          case _ =>
                            required = required | math.pow(2, oneField.index).toInt // required
                            oneField.fieldRef.unitVal.asTerm
                        }
                        (ValDef(sym, Some(unitVal)), caseDef, fieldSymRef)
                      else
                        val methodSymbol = dvMembers.head
                        val dvSelectNoTArgs = Ref(companionModule).select(methodSymbol)
                        val dvSelect = methodSymbol.paramSymss match
                          case Nil => dvSelectNoTArgs
                          case List(params) if (params.exists(_.isTypeParam)) =>
                            typeArgs(tpe) match
                              case Nil      => ??? // throw JsonParseError("Expected an applied type", ???)
                              case typeArgs => TypeApply(dvSelectNoTArgs, typeArgs.map(Inferred(_)))
                          case _ => ??? // fail(s"Default method for ${symbol.name} of class ${tpe.show} have a complex " +
                        (ValDef(sym, Some(dvSelect)), caseDef, fieldSymRef)
                  }
                }
                val reqVarDef = ValDef(reqSym, Some(Literal(IntConstant(totalRequired))))
                val (varDefs, caseDefs, idents) = together.unzip3
                val caseDefsWithFinal = caseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)

                val argss = List(idents)
                val primaryConstructor = tpe.classSymbol.get.primaryConstructor
                val constructorNoTypes = Select(New(Inferred(tpe)), primaryConstructor)
                val constructor = typeArgs(tpe) match
                  case Nil      => constructorNoTypes
                  case typeArgs => TypeApply(constructorNoTypes, typeArgs.map(Inferred(_)))
                val instantiateClass = argss.tail.foldLeft(Apply(constructor, argss.head))((acc, args) => Apply(acc, args))

                val exprRequired = Expr(required)

                makeClassFieldMatrixValDef(MethodKey(t, false), t.name.replaceAll("\\.", "_"), t.fields.map(_.name).toArray)
                val fieldMatrixSym = classFieldMatrixSyms(MethodKey(t, false)).asInstanceOf[Symbol]

                val parseLoop = '{
                  var maybeFieldNum = $in.expectFirstObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })
                  if maybeFieldNum == null then null.asInstanceOf[T]
                  else
                    while maybeFieldNum.isDefined do
                      ${ Match('{ maybeFieldNum.get }.asTerm, caseDefsWithFinal).asExprOf[Any] }
                      maybeFieldNum = $in.expectObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })
                    if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then ${ instantiateClass.asExprOf[T] }
                    else throw new JsonParseError("Missing required field(s) " + ${ allFieldNames }(Integer.numberOfTrailingZeros(${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired })), $in)
                }.asTerm

                Block(varDefs :+ reqVarDef, parseLoop).asExprOf[T]
              )

            case t => throw new ParseError("Not yet implemented: " + t)

    // ---------------------------------------------------------------------------------------------

    def genReadVal[T: Type](
        // default: Expr[T], // needed?  This should already be in ref...
        ref: RTypeRef[T],
        in: Expr[JsonSource],
        inTuple: Boolean = false, // not sure if needed...
        isMapKey: Boolean = false
    )(using Quotes): Expr[T] =
      val methodKey = MethodKey(ref, false)
      readMethodSyms
        .get(methodKey)
        .map { sym => // hit cache first... then match on Ref type
          Apply(Ref(sym), List(in.asTerm)).asExprOf[T]
        }
        .getOrElse(
          ref match
            // First cover all primitive and simple types...
            case t: BigDecimalRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case s    => scala.math.BigDecimal(s)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case s    => scala.math.BigDecimal(s)
                }.asExprOf[T]
            case t: BigIntRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case s    => scala.math.BigInt(s)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case s    => scala.math.BigInt(s)
                }.asExprOf[T]
            case t: BooleanRef =>
              if isMapKey then '{ $in.expectString().toBoolean }.asExprOf[T]
              else '{ $in.expectBoolean() }.asExprOf[T]
            case t: ByteRef =>
              if isMapKey then '{ $in.expectString().toInt.toByte }.asExprOf[T]
              else '{ $in.expectInt().toByte }.asExprOf[T]
            case t: CharRef =>
              '{
                $in.expectString() match
                  case null =>
                    $in.backspace()
                    $in.backspace()
                    $in.backspace()
                    $in.backspace()
                    throw JsonParseError("Char value cannot be null", $in)
                  case "" =>
                    $in.backspace()
                    $in.backspace()
                    throw JsonParseError("Char value expected but empty string found in json", $in)
                  case c => c.charAt(0)
              }.asExprOf[T]
            case t: DoubleRef =>
              if isMapKey then '{ $in.expectString().toDouble }.asExprOf[T]
              else '{ $in.expectDouble() }.asExprOf[T]
            case t: FloatRef =>
              if isMapKey then '{ $in.expectString().toFloat }.asExprOf[T]
              else '{ $in.expectFloat() }.asExprOf[T]
            case t: IntRef =>
              if isMapKey then '{ $in.expectString().toInt }.asExprOf[T]
              else '{ $in.expectInt() }.asExprOf[T]
            case t: LongRef =>
              if isMapKey then '{ $in.expectString().toLong }.asExprOf[T]
              else '{ $in.expectLong() }.asExprOf[T]
            case t: ShortRef =>
              if isMapKey then '{ $in.expectString().toShort }.asExprOf[T]
              else '{ $in.expectInt().toShort }.asExprOf[T]
            case t: StringRef =>
              '{ $in.expectString() }.asExprOf[T]

            case t: JBigDecimalRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => new java.math.BigDecimal(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => new java.math.BigDecimal(n)
                }.asExprOf[T]
            case t: JBigIntegerRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => new java.math.BigInteger(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => new java.math.BigInteger(n)
                }.asExprOf[T]
            case t: JBooleanRef =>
              if isMapKey then '{ java.lang.Boolean.valueOf($in.expectString()) }.asExprOf[T]
              else '{ $in.expectJavaBoolean() }.asExprOf[T]
            case t: JByteRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Byte.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Byte.valueOf(n)
                }.asExprOf[T]
            case t: JCharacterRef =>
              '{
                val c = $in.expectString()
                if c == null then null
                else if c.length == 0 then
                  $in.backspace()
                  $in.backspace()
                  throw JsonParseError("Character value expected but empty string found in json", $in)
                else java.lang.Character.valueOf(c.charAt(0))
              }.asExprOf[T]
            case t: JDoubleRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Double.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Double.valueOf(n)
                }.asExprOf[T]
            case t: JFloatRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Float.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Float.valueOf(n)
                }.asExprOf[T]
            case t: JIntegerRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Integer.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Integer.valueOf(n)
                }.asExprOf[T]
            case t: JLongRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Long.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Long.valueOf(n)
                }.asExprOf[T]
            case t: JShortRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n    => java.lang.Short.valueOf(n)
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n    => java.lang.Short.valueOf(n)
                }.asExprOf[T]
            case t: JNumberRef =>
              if isMapKey then
                '{
                  $in.expectString() match
                    case null => null
                    case n =>
                      scala.math.BigDecimal(n) match {
                        case d if d.isValidByte     => java.lang.Byte.valueOf(d.toByteExact)
                        case d if d.isValidShort    => java.lang.Short.valueOf(d.toShortExact)
                        case d if d.isValidInt      => java.lang.Integer.valueOf(d.toIntExact)
                        case d if d.isValidLong     => java.lang.Long.valueOf(d.toLongExact)
                        case d if d.isDecimalFloat  => java.lang.Float.valueOf(d.toFloat)
                        case d if d.isDecimalDouble => java.lang.Double.valueOf(d.toDouble)
                        case d                      => d
                      }
                }.asExprOf[T]
              else
                '{
                  $in.expectNumberOrNull() match
                    case null => null
                    case n =>
                      scala.math.BigDecimal(n) match {
                        case d if d.isValidByte     => java.lang.Byte.valueOf(d.toByteExact)
                        case d if d.isValidShort    => java.lang.Short.valueOf(d.toShortExact)
                        case d if d.isValidInt      => java.lang.Integer.valueOf(d.toIntExact)
                        case d if d.isValidLong     => java.lang.Long.valueOf(d.toLongExact)
                        case d if d.isDecimalFloat  => java.lang.Float.valueOf(d.toFloat)
                        case d if d.isDecimalDouble => java.lang.Double.valueOf(d.toDouble)
                        case d                      => d
                      }
                }.asExprOf[T]

            case t: DurationRef       => '{ $in.expectString(java.time.Duration.parse) }.asExprOf[T]
            case t: InstantRef        => '{ $in.expectString(java.time.Instant.parse) }.asExprOf[T]
            case t: LocalDateRef      => '{ $in.expectString(java.time.LocalDate.parse) }.asExprOf[T]
            case t: LocalDateTimeRef  => '{ $in.expectString(java.time.LocalDateTime.parse) }.asExprOf[T]
            case t: LocalTimeRef      => '{ $in.expectString(java.time.LocalTime.parse) }.asExprOf[T]
            case t: MonthDayRef       => '{ $in.expectString(java.time.MonthDay.parse) }.asExprOf[T]
            case t: OffsetDateTimeRef => '{ $in.expectString(java.time.OffsetDateTime.parse) }.asExprOf[T]
            case t: OffsetTimeRef     => '{ $in.expectString(java.time.OffsetTime.parse) }.asExprOf[T]
            case t: PeriodRef         => '{ $in.expectString(java.time.Period.parse) }.asExprOf[T]
            case t: YearRef           => '{ $in.expectString(java.time.Year.parse) }.asExprOf[T]
            case t: YearMonthRef      => '{ $in.expectString(java.time.YearMonth.parse) }.asExprOf[T]
            case t: ZonedDateTimeRef  => '{ $in.expectString(java.time.ZonedDateTime.parse) }.asExprOf[T]
            case t: ZoneIdRef         => '{ $in.expectString(java.time.ZoneId.of) }.asExprOf[T]
            case t: ZoneOffsetRef     => '{ $in.expectString(java.time.ZoneOffset.of) }.asExprOf[T]

            case t: URLRef  => '{ $in.expectString((s: String) => new java.net.URL(s)) }.asExprOf[T]
            case t: URIRef  => '{ $in.expectString((s: String) => new java.net.URI(s)) }.asExprOf[T]
            case t: UUIDRef => '{ $in.expectString(java.util.UUID.fromString) }.asExprOf[T]

            case t: AliasRef[?] =>
              // Special check for RawJson pseudo-type
              if lastPart(t.definedType) == "RawJson" then
                '{
                  $in.mark()
                  $in.skipValue()
                  $in.captureMark()
                }.asExprOf[T]
              else
                t.unwrappedType.refType match
                  case '[e] =>
                    '{
                      ${
                        genReadVal[e](
                          t.unwrappedType.asInstanceOf[RTypeRef[e]],
                          in,
                          inTuple,
                          isMapKey
                        )
                      }.asInstanceOf[T]
                    }

            // --------------------
            //  Options...
            // --------------------
            case t: OptionRef[?] =>
              import quotes.reflect.*
              t.optionParamType.refType match
                case '[e] =>
                  if cfg.noneAsNull || inTuple then
                    '{
                      if $in.expectNull() then None
                      else Some(${ genReadVal[e](t.optionParamType.asInstanceOf[RTypeRef[e]], in) })
                    }.asExprOf[T]
                  else ofOption[e](Some(genReadVal[e](t.optionParamType.asInstanceOf[RTypeRef[e]], in))).asExprOf[T]

            case t: LeftRightRef[?] if t.lrkind == LRKind.EITHER =>
              import quotes.reflect.*
              t.leftRef.refType match
                case '[l] =>
                  t.rightRef.refType match
                    case '[r] =>
                      '{
                        $in.mark()
                        if $in.expectNull() then null
                        else
                          scala.util.Try(${ genReadVal[r](t.rightRef.asInstanceOf[RTypeRef[r]], in, inTuple) }) match
                            case Success(rval) =>
                              Right(rval)
                            case Failure(f) =>
                              $in.revertToMark()
                              scala.util.Try(${ genReadVal[l](t.leftRef.asInstanceOf[RTypeRef[l]], in, inTuple) }) match
                                case Success(lval) => Left(lval)
                                case Failure(_) =>
                                  $in.backspace()
                                  throw JsonParseError("Failed to read either side of Either type", $in)
                      }.asExprOf[T]

            case t: LeftRightRef[?] if t.lrkind == LRKind.UNION =>
              import quotes.reflect.*
              t.leftRef.refType match
                case '[l] =>
                  t.rightRef.refType match
                    case '[r] =>
                      '{
                        $in.mark()
                        scala.util.Try(${ genReadVal[l](t.leftRef.asInstanceOf[RTypeRef[l]], in, true) }) match
                          case Success(lval) => lval
                          case Failure(f) =>
                            $in.revertToMark()
                            scala.util.Try(${ genReadVal[r](t.rightRef.asInstanceOf[RTypeRef[r]], in, true) }) match
                              case Success(rval) => rval
                              case Failure(_) =>
                                $in.backspace()
                                throw JsonParseError("Failed to read either side of Union type", $in)
                      }.asExprOf[T]
            /*
            TODO
              Intersection:
val syntheticTA = taCache.typeAdapterOf[L]
syntheticTA.write(t.asInstanceOf[L], writer, out)

              Union:
  def read(parser: Parser): L | R = {
    val savedReader = parser.mark()
    Try(leftTypeAdapter.read(parser)) match {
      case Success(leftValue) => leftValue.asInstanceOf[L]
      case Failure(_) => // Left parse failed... try Right
        parser.revertToMark(savedReader)
        Try(rightTypeAdapter.read(parser)) match {
          case Success(rightValue) => rightValue.asInstanceOf[R]
          case Failure(x) =>
            parser.backspace()
            throw new ScalaJackError( parser.showError(s"Failed to read any values for union type") )
        }
    }
  }
             */

            // --------------------
            //  Enumerations...
            // --------------------
            case t: ScalaEnumRef[?] =>
              import quotes.reflect.*
              t.refType match
                case '[e] =>
                  '{
                    $in.expectEnum() match
                      case null => null
                      case s: String =>
                        ${
                          val typeRepr = TypeRepr.of[e]
                          val m = typeRepr.typeSymbol.companionClass.declaredMethod("valueOf")
                          Apply(Ref(m(0)), List('{ s }.asTerm)).asExpr
                        }
                      case v: Int =>
                        ${
                          val typeRepr = TypeRepr.of[e]
                          val m = typeRepr.typeSymbol.companionClass.declaredMethod("fromOrdinal")
                          Apply(Ref(m(0)), List('{ v }.asTerm)).asExpr
                        }
                  }.asExprOf[T]
            case t: ScalaEnumerationRef[?] =>
              import quotes.reflect.*
              t.refType match
                case '[e] =>
                  '{
                    $in.expectEnum() match
                      case null => null
                      case s: String =>
                        ${
                          val enumeration = TypeRepr.of[e] match
                            case TypeRef(ct, _) => Ref(ct.termSymbol).asExprOf[Enumeration]
                          '{ ${ enumeration }.values.iterator.find(_.toString == s).get }.asExprOf[e]
                        }
                      case v: Int =>
                        ${
                          val enumeration = TypeRepr.of[e] match
                            case TypeRef(ct, _) => Ref(ct.termSymbol).asExprOf[Enumeration]
                          '{ ${ enumeration }.values.iterator.find(_.id == v).get }.asExprOf[e]
                        }
                  }.asExprOf[T]
            case t: JavaEnumRef[?] =>
              import quotes.reflect.*
              t.refType match
                case '[e] =>
                  '{
                    $in.expectEnum() match
                      case null => null
                      case s: String =>
                        ${
                          val typeRepr = TypeRepr.of[e]
                          val m = typeRepr.classSymbol.get.companionModule.methodMember("valueOf")
                          Apply(Ref(m(0)), List('{ s }.asTerm)).asExpr
                        }
                      case v: Int =>
                        throw JsonParseError("Ordinal value initiation not valid for Java Enums", $in)
                  }.asExprOf[T]

            // --------------------
            //  Collections...
            // --------------------
            case t: SeqRef[?] =>
              ref.refType match
                case '[List[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.toList
                        else null
                      }.asExprOf[T]
                case '[Vector[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.toVector
                        else null
                      }.asExprOf[T]
                case '[Seq[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.toSeq
                        else null
                      }.asExprOf[T]
                case '[IndexedSeq[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.toIndexedSeq
                        else null
                      }.asExprOf[T]
                case '[Iterable[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.toIterable
                        else null
                      }.asExprOf[T]
                // Catch all, with (slightly) slower type coersion to proper Seq flavor
                case _ =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        if parsedArray != null then parsedArray.to(${ Expr.summon[Factory[e, T]].get }) // create appropriate flavor of Seq[T] here
                        else null
                      }.asExprOf[T]

            case t: ArrayRef[?] =>
              t.elementRef.refType match
                case '[e] =>
                  val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                  val ct = Expr.summon[ClassTag[e]].get
                  '{
                    val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](rtypeRef, in, inTuple) })
                    if parsedArray != null then
                      implicit val ctt = $ct
                      parsedArray.toArray[e]
                    else null
                  }.asExprOf[T]

            case t: MapRef[?] =>
              t.elementRef.refType match
                case '[k] =>
                  t.elementRef2.refType match
                    case '[v] =>
                      testValidMapKey(t.elementRef)
                      '{
                        if $in.expectNull() then null
                        else
                          $in.expectToken('{')
                          $in.parseMap[k, v](
                            () => ${ genReadVal[k](t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                            () => ${ genReadVal[v](t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
                            Map.empty[k, v],
                            true
                          )
                      }.asExprOf[T]

            // --------------------
            //  Tuples...
            // --------------------
            case t: TupleRef[?] =>
              import quotes.reflect.*
              t.refType match
                case '[tt] =>
                  val tpe = TypeRepr.of[tt]
                  val maxI = Expr(t.tupleRefs.length - 1)
                  val indexedTypes = tpe match
                    case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
                    case _                        => Nil

                  // make all the tuple terms, accounting for , and ] detection
                  val tupleTerms =
                    if t.tupleRefs.length == 1 then
                      t.tupleRefs(0).refType match
                        case '[e] =>
                          List('{
                            ${ genReadVal[e](t.tupleRefs(0).asInstanceOf[RTypeRef[e]], in, true) }
                            $in.expectToken(']')
                          }.asTerm)
                    else
                      t.tupleRefs.zipWithIndex.map { case (tpart, i) =>
                        tpart.refType match
                          case '[e] =>
                            if i == 0 then genReadVal[e](tpart.asInstanceOf[RTypeRef[e]], in, true).asTerm
                            else
                              '{
                                $in.expectToken(',')
                                ${ genReadVal[e](tpart.asInstanceOf[RTypeRef[e]], in, true) }
                              }.asTerm
                      }
                  '{
                    if $in.expectNull() then null
                    else
                      $in.expectToken('[')
                      val tv: T = ${
                        Apply(TypeApply(Select.unique(New(Inferred(tpe)), "<init>"), indexedTypes.map(x => Inferred(x))), tupleTerms).asExprOf[T]
                      }
                      $in.expectToken(']')
                      tv
                  }.asExprOf[T]

            case _ =>
              // Classes, traits, etc.
              genDecFnBody[T](ref, in) // Create a new decoder function (presumably for class, trait, etc)
              genReadVal(ref, in, inTuple)

          // Just-created function is present now and will be called
        )

    // ---------------------------------------------------------------------------------------------

    // ================================================================
    // You've made it this far!  Ok, now we sew everything together.
    // We generate a codec class and then kick off a deep traversal of
    // generation from the given root ref (refer waaay back at the top of this fn...).
    // ================================================================
    val codecDef = '{ // FIXME: generate a type class instance using `ClassDef.apply` and `Symbol.newClass` calls after graduating from experimental API: https://www.scala-lang.org/blog/2022/06/21/scala-3.1.3-released.html
      new JsonCodec[T] {
        // def nullValue: A = ${genNullValue[A](rootTpe :: Nil)} // <- needed?

        // TBD... when we're ready to tackle reading!
        // def decodeValue(in: JsonReader, default: A): A = ${
        //     if (cfg.encodingOnly) '{ ??? }
        //     else genReadVal(rootTpe :: Nil, 'default, cfg.isStringified, false, 'in)
        // }

        def encodeValue(in: T, out: JsonOutput): Unit = ${ genWriteVal('in, ref, 'out) }
        def decodeValue(in: JsonSource): T = ${ genReadVal(ref, 'in) }
      }
    }.asTerm
    val neededDefs =
      // others here???  Refer to Jsoniter file JsonCodecMaker.scala
      classFieldMatrixValDefs ++ writeMethodDefs ++ readMethodDefs
    val codec = Block(neededDefs.toList, codecDef).asExprOf[JsonCodec[T]]
    // println(s"Codec: ${codec.show}")
    codec
