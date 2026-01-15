package co.blocke.scalajack
package json
package writing

import scala.quoted.*
import scala.jdk.CollectionConverters.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.rtypes.{EnumRType, JavaClassRType, NonConstructorFieldInfo}
import shared.*

object Writer:

  private def makeWriteFnSymbol[U: Type](
      ctx: CodecBuildContext,
      methodKey: TypedName
  ): Unit =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*
    val _ = ctx.writeMethodSyms.getOrElseUpdate(
      methodKey,
      Symbol.newMethod(
        Symbol.spliceOwner,
        "w" + ctx.writeMethodSyms.size,
        MethodType(List("in", "out"))(
          _ => List(TypeRepr.of[U], TypeRepr.of[JsonOutput]),
          _ => TypeRepr.of[Unit]
        )
      )
    )

  private def makeWriteFn[U: Type](
      ctx: CodecBuildContext,
      methodKey: TypedName,
      arg: Expr[U],
      out: Expr[JsonOutput]
  )(f: (Expr[U], Expr[JsonOutput]) => Expr[Unit]): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val writeMethodSym = ctx.writeMethodSyms.getOrElse(methodKey, throw new TypeError(s"Missing write fn symbol for $methodKey"))
    ctx.writeMethodDefs.update(
      methodKey,
      DefDef(
        writeMethodSym,
        params => {
          val List(List(in, out)) = params
          Some(f(in.asExprOf[U], out.asExprOf[JsonOutput]).asTerm.changeOwner(writeMethodSym))
        }
      )
    )

    // Always apply the function
    Apply(Ref(writeMethodSym), List(arg.asTerm, out.asTerm)).asExprOf[Unit]

// ---------------------------------------------------------------------------------------------

//  Writing (encoding) JSON is accomplished with two functions: genEncFnBody() and genWriteVal(). The former is the primary entry for writing some
//  JSON. As part of writing it is common to write a value, for example the elements of a list. genWriteVal() handles this tedium, including recursively
//  calling genEncFnBody() if needed for more complex object.s

// ---------------------------------------------------------------------------------------------

  // So why 2 functions for writing? It boils down to this:
  // If, in the generated output code, you need a function generated to read something (typically a complex thing)
  // you call genEncFnBody, otherwise call genWriteVal, which generates inline writing code.
  private def genEncFnBody[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      r: RTypeRef[?],
      aE: Expr[T],
      out: Expr[JsonOutput],
      emitDiscriminator: Expr[Boolean], // caller must pass this default ==> '{ false },
      inTuple: Boolean = false,
      isMapKey: Boolean = false
  ): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val methodKey = r.typedName
    r.refType match
      case '[b] =>
        r match
          // Basically sealed traits...
          case t: Sealable if t.isSealed =>
            makeWriteFnSymbol(ctx, methodKey)
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              if t.childrenAreObject then
                '{
                  if $in == null then $out.burpNull()
                  else $out.value($in.getClass.getName.split('.').last.stripSuffix("$"))
                }
              else
                val unique = Unique.findUniqueWithExcluded(t)(ctx)
                val cases = t.sealedChildren.map { child =>
                  child.refType match
                    case '[c] =>
                      // Figure out if class fields are unique: If t.uniqueFields(hash) has length 1 and that 1 is this subtype
                      val renderHint: Expr[Boolean] =
                        if !cfg._preferTypeHints then
                          child match {
                            case sr: ScalaClassRef[c] =>
                              Expr(unique.needsTypeHint(sr.fields.map(_.name)))
                            case sr: TraitRef[c] =>
                              val fingerprintByClass = unique.fingerprintByClass // Classname->Option[Fingerprint]
                              val liftedFingerprintByClass = liftStringOptionMap(fingerprintByClass)
                              '{
                                val classFingerprint: Option[String] = $liftedFingerprintByClass($aE.getClass.getName)
                                classFingerprint.isEmpty // no fingerprint = needs hint
                              }.asExprOf[Boolean]
                            case _ => '{ true }
                          }
                        else '{ true }
                      val subtype = TypeRepr.of[c]
                      val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, subtype)
                      CaseDef(
                        Bind(sym, Typed(Wildcard(), Inferred(subtype))),
                        None,
                        genEncFnBody[c](ctx, cfg, child, Ref(sym).asExprOf[c], out, renderHint, inTuple = inTuple, isMapKey = isMapKey).asTerm
                      )
                } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                Match(in.asTerm, cases).asExprOf[Unit]
            }

          // We don't use makeWriteFn here because a value class is basically just a "box" around a simple type
          case t: ScalaClassRef[?] if t.isValueClass =>
            val theField = t.fields.head.fieldRef
            theField.refType match
              case '[e] =>
                val fieldValue = Select.unique(aE.asTerm, t.fields.head.name).asExprOf[e]
                genWriteVal(ctx, cfg, fieldValue, theField.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple, isMapKey = isMapKey)

          case t: ScalaClassRef[?] =>
            makeWriteFnSymbol(ctx, methodKey)
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              val body = {
                val eachField = t.fields.map { f =>
                  f.fieldRef.refType match
                    case '[z] =>
                      val fieldValue = Select.unique(in.asTerm, f.name).asExprOf[z]
                      val fieldName = changeFieldName(f)
                      MaybeWrite.maybeWrite[z](ctx, cfg, fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out)
                }
                //  -- To block.... (soak this to see if it works then delete old block
                val cname: Expr[String] = cfg.typeHintPolicy match
                  case TypeHintPolicy.SIMPLE_CLASSNAME =>
                    Expr(lastPart(t.name))
                  case TypeHintPolicy.SCRAMBLE_CLASSNAME =>
                    '{ scramble(${ Expr(lastPart(t.name).hashCode) }) }
                  case TypeHintPolicy.USE_ANNOTATION =>
                    Expr(
                      t.annotations
                        .get("co.blocke.scalajack.TypeHint")
                        .flatMap(_.get("hintValue"))
                        .getOrElse(lastPart(t.name))
                    )

                val discExpr: Expr[Unit] = '{
                  $out.label(${ Expr(cfg.typeHintLabel) })
                  $out.value($cname)
                }

                val discBlock: Expr[Unit] = {
                  val withDisc: List[Expr[Unit]] = discExpr +: eachField
                  withDisc match
                    case Nil      => '{ () }
                    case x :: Nil => x
                    case xs       => Expr.block(xs.init, xs.last)
                }

                val noDiscBlock: Expr[Unit] =
                  eachField match
                    case Nil      => '{ () }
                    case x :: Nil => x
                    case xs       => Expr.block(xs.init, xs.last)

                // Use quote-level If expression
                If(
                  cond = emitDiscriminator.asTerm,
                  thenp = discBlock.asTerm,
                  elsep = noDiscBlock.asTerm
                ).asExprOf[Unit]
              }

              if !t.isCaseClass && cfg._writeNonConstructorFields then
                val eachField = t.nonConstructorFields.map { f =>
                  f.fieldRef.refType match
                    case '[e] =>
                      val fieldValue = Select.unique(in.asTerm, f.getterLabel).asExprOf[e]
                      val fieldName = changeFieldName(f)
                      MaybeWrite.maybeWrite[e](ctx, cfg, fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[e]], out)
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

          case t: JavaClassRef[?] =>
            makeWriteFnSymbol(ctx, methodKey)
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              t.refType match
                case '[p] =>
                  val rtype = t.expr.asExprOf[JavaClassRType[p]]
                  val tin = in.asExprOf[b]
                  var fieldRefs = t.fields.asInstanceOf[List[NonConstructorFieldInfoRef]]
                  val sref = ReflectOnType[String](ctx.quotes)(TypeRepr.of[String])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
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
                        val fieldName = field.annotations.get("co.blocke.scalajack.jsLabel").flatMap(_.get("name")).getOrElse(f.name)
                        ${
                          val ref = fieldRefs.head
                          fieldRefs = fieldRefs.tail
                          ref.fieldRef.refType match
                            case '[e] =>
                              MaybeWrite.maybeWriteMap[String, e](
                                ctx,
                                cfg,
                                '{ fieldName },
                                '{ fieldValue.asInstanceOf[e] },
                                sref,
                                ref.fieldRef.asInstanceOf[RTypeRef[e]],
                                out
                              )
                        }
                      }
                      $out.endObject()
                  }
            }

          case t: TraitRef[?] => throw UnsupportedType("Non-sealed traits are not supported")

          case s: SelfRefRef[?] =>
            s.refType match
              case '[e] =>
                val tin = aE.asExprOf[T]
                '{
                  val vv = $tin.asInstanceOf[e]
                  ${
                    genWriteVal[e](
                      ctx,
                      cfg,
                      '{ vv },
                      co.blocke.scala_reflection.reflect.ReflectOnType[e](ctx.quotes)(TypeRepr.of[e], true)(using ctx.seenBefore),
                      out,
                      inTuple = inTuple
                    )
                  }
                }

          case t =>
            throw new UnsupportedType("Type represented by " + t.name + " is unsupported for JSON writes")

  // ---------------------------------------------------------------------------------------------

  def genWriteVal[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      aE: Expr[T],
      ref: RTypeRef[T],
      out: Expr[JsonOutput],
      inTuple: Boolean = false,
      isMapKey: Boolean = false // only primitive or primitive-equiv types can be Map keys
  ): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val methodKey = ref.typedName
    ctx.writeMethodSyms
      .get(methodKey)
      .map { sym => // hit cache first... then match on Ref type
        Apply(Ref(sym), List(aE.asTerm, out.asTerm)).asExprOf[Unit]
      }
      .getOrElse(
        Expr.summon[JsonCodec[T]] match {
          case Some(userOverride) => '{ ${ userOverride }.encodeValue($aE, $out) }
          case None =>
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

              case t: StringRef =>
                if cfg._suppressEscapedStrings then '{ $out.value(${ aE.asExprOf[String] }) }
                else '{ $out.valueEscaped(${ aE.asExprOf[String] }) }

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

              case t: URLRef       => '{ $out.value(${ aE.asExprOf[java.net.URL] }) }
              case t: URIRef       => '{ $out.value(${ aE.asExprOf[java.net.URI] }) }
              case t: UUIDRef      => '{ $out.value(${ aE.asExprOf[java.util.UUID] }) }
              case t: ObjectRef[?] => '{ $out.value(${ Expr(t.name) }) }

              case t: AliasRef[?] =>
                t.unwrappedType.refType match
                  case '[e] =>
                    genWriteVal[e](ctx, cfg, aE.asInstanceOf[Expr[e]], t.unwrappedType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple)

              case t: ArrayRef[?] =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = aE.asInstanceOf[Expr[Array[e]]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        }
                        $out.endArray()
                    }

              case t: IterableRef[?] =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = aE.asInstanceOf[Expr[Iterable[e]]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        }
                        $out.endArray()
                    }

              case t: SeqRef[?] =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Seq[e]] else aE.asExprOf[Seq[e]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        }
                        $out.endArray()
                    }

              case t: SetRef[?] =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Set[e]] else aE.asExprOf[Set[e]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.foreach { i =>
                          ${ genWriteVal(ctx, cfg, '{ i }.asExprOf[e], t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        }
                        $out.endArray()
                    }

              // These are here becaue Enums and their various flavors can be Map keys
              // (EnumRef handles: Scala 3 enum, Scala 2 Enumeration, Java Enumeration)
              case t: EnumRef[?] =>
                val enumAsId = cfg.enumsAsIds match
                  case List("-")                     => false
                  case Nil                           => true
                  case list if list.contains(t.name) => true
                  case _                             => false
                val rtype = t.expr
                if enumAsId then
                  if isMapKey then '{ $out.value($rtype.asInstanceOf[EnumRType[?]].ordinal($aE.toString).get.toString) } // stringified id
                  else '{ $out.value($rtype.asInstanceOf[EnumRType[?]].ordinal($aE.toString).get) } // int value of id
                else '{ if $aE == null then $out.burpNull() else $out.value($aE.toString) }

              // No makeWriteFn here--Option is just a wrapper to the real thingy
              case t: ScalaOptionRef[?] =>
                t.optionParamType.refType match
                  case '[e] =>
                    val tin = aE.asExprOf[T]
                    '{
                      $tin match
                        case null => $out.burpNull()
                        case None =>
                          ${
                            if cfg.noneAsNull || inTuple then '{ $out.burpNull() }
                            else '{ () }
                          }
                        case Some(v) =>
                          val vv = v.asInstanceOf[e]
                          ${ genWriteVal[e](ctx, cfg, '{ vv }, t.optionParamType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }

              case t: JavaOptionalRef[?] =>
                t.optionParamType.refType match
                  case '[e] =>
                    val tin = aE.asExprOf[T]
                    '{
                      $tin.asInstanceOf[java.util.Optional[e]] match
                        case null => $out.burpNull()
                        case o if !o.isPresent =>
                          ${
                            if cfg.noneAsNull || inTuple then '{ $out.burpNull() }
                            else '{ () }
                          }
                        case o =>
                          val vv = o.get().asInstanceOf[e]
                          ${ genWriteVal[e](ctx, cfg, '{ vv }, t.optionParamType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }

              // No makeWriteFn here.  All LeftRight types (Either, Union, Intersection) are just type wrappers
              case t: LeftRightRef[?] =>
                val tin = aE.asExprOf[T]
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
                                      ${ genWriteVal[lt](ctx, cfg, '{ v.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, inTuple = inTuple) }
                                    case Right(v) =>
                                      ${ genWriteVal[rt](ctx, cfg, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                              }
                            case EitherLeftPolicy.AS_NULL =>
                              '{
                                if $tin == null then $out.burpNull()
                                else
                                  $tin match
                                    case Left(v) => $out.burpNull()
                                    case Right(v) =>
                                      ${ genWriteVal[rt](ctx, cfg, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                              }
                            case EitherLeftPolicy.ERR_MSG_STRING =>
                              '{
                                if $tin == null then $out.burpNull()
                                else
                                  $tin match
                                    case Left(v) => $out.value("Left Error: " + v.toString)
                                    case Right(v) =>
                                      ${ genWriteVal[rt](ctx, cfg, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                              }
                            case EitherLeftPolicy.THROW_EXCEPTION =>
                              '{
                                if $tin == null then $out.burpNull()
                                else
                                  $tin match
                                    case Left(v) => throw new EitherLeftError("Left Error: " + v.toString)
                                    case Right(v) =>
                                      ${ genWriteVal[rt](ctx, cfg, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                              }
                        else
                          '{
                            $out.mark()
                            scala.util.Try {
                              ${ genWriteVal[rt](ctx, cfg, '{ $tin.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                            } match
                              case scala.util.Success(_) => () // do nothing further--write to out already happened
                              case scala.util.Failure(_) =>
                                $out.revert()
                                ${ genWriteVal[lt](ctx, cfg, '{ $tin.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out, inTuple = inTuple) }
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
                            ${ genWriteVal[e](ctx, cfg, '{ v }.asInstanceOf[Expr[e]], t.tryRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                          case scala.util.Failure(v) =>
                            ${
                              cfg.tryFailureHandling match
                                case _ if inTuple              => '{ $out.burpNull() }
                                case TryPolicy.AS_NULL         => '{ $out.burpNull() }
                                case TryPolicy.ERR_MSG_STRING  => '{ $out.value("Try Failure with msg: " + v.getMessage) }
                                case TryPolicy.THROW_EXCEPTION => '{ throw v }
                            }
                    }

              case t: MapRef[?] =>
                t.elementRef.refType match
                  case '[k] =>
                    t.elementRef2.refType match
                      case '[v] =>
                        val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Map[k, v]] else aE.asExprOf[Map[k, v]]
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
                                            MaybeWrite.maybeWriteMap[ak, av](
                                              ctx,
                                              cfg,
                                              '{ key.asInstanceOf[ak] },
                                              '{ value.asInstanceOf[av] },
                                              aliasK.unwrappedType.asInstanceOf[RTypeRef[ak]],
                                              aliasV.unwrappedType.asInstanceOf[RTypeRef[av]],
                                              out
                                            )
                                  case (_, aliasV: AliasRef[?]) =>
                                    aliasV.unwrappedType.refType match
                                      case '[av] =>
                                        testValidMapKey(t.elementRef)
                                        MaybeWrite.maybeWriteMap[k, av](
                                          ctx,
                                          cfg,
                                          '{ key }.asExprOf[k],
                                          '{ value.asInstanceOf[av] },
                                          t.elementRef.asInstanceOf[RTypeRef[k]],
                                          aliasV.unwrappedType.asInstanceOf[RTypeRef[av]],
                                          out
                                        )
                                  case (aliasK: AliasRef[?], _) =>
                                    aliasK.unwrappedType.refType match
                                      case '[ak] =>
                                        testValidMapKey(aliasK.unwrappedType)
                                        MaybeWrite.maybeWriteMap[ak, v](
                                          ctx,
                                          cfg,
                                          '{ key.asInstanceOf[ak] },
                                          '{ value }.asExprOf[v],
                                          aliasK.unwrappedType.asInstanceOf[RTypeRef[ak]],
                                          t.elementRef2.asInstanceOf[RTypeRef[v]],
                                          out
                                        )
                                  case (_, _) =>
                                    testValidMapKey(t.elementRef)
                                    MaybeWrite.maybeWriteMap[k, v](
                                      ctx,
                                      cfg,
                                      '{ key },
                                      '{ value }.asExprOf[v],
                                      t.elementRef.asInstanceOf[RTypeRef[k]],
                                      t.elementRef2.asInstanceOf[RTypeRef[v]],
                                      out
                                    )
                              }
                            }
                            $out.endObject()
                        }

              case t: JavaCollectionRef[?] =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = '{ $aE.asInstanceOf[java.util.Collection[?]] }
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startArray()
                        $tin.toArray.foreach { elem =>
                          ${ genWriteVal(ctx, cfg, '{ elem.asInstanceOf[e] }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                        }
                        $out.endArray()
                    }

              case t: JavaMapRef[?] =>
                t.elementRef.refType match
                  case '[k] =>
                    t.elementRef2.refType match
                      case '[v] =>
                        val tin = aE.asExprOf[java.util.Map[k, v]]
                        '{
                          if $tin == null then $out.burpNull()
                          else
                            $out.startObject()
                            $tin.asScala.foreach { case (key, value) =>
                              ${
                                MaybeWrite.maybeWriteMap[k, v](
                                  ctx,
                                  cfg,
                                  '{ key },
                                  '{ value }.asExprOf[v],
                                  t.elementRef.asInstanceOf[RTypeRef[k]],
                                  t.elementRef2.asInstanceOf[RTypeRef[v]],
                                  out
                                )
                              }
                            }
                            $out.endObject()
                        }

              case t: TupleRef[?] =>
                val in = aE.asExprOf[T]
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
                            genWriteVal[e](ctx, cfg, fieldValue, ref.asInstanceOf[RTypeRef[e]], out, inTuple = true)
                      }
                      if elementsE.size == 1 then elementsE.head
                      else Expr.block(elementsE.init, elementsE.last)
                    }
                    $out.endArray()
                }

              // NeoType is a bit of a puzzle-box.  To get the correct underlying base type, I had to dig into
              // the argument of method validate$retainedBody.  It happened to have the correctly-typed parameter.
              // With the correct type, we can correct write out the value.
              case t: NeoTypeRef[?] => // in Quotes context
                Symbol.requiredModule(t.typedName.toString).methodMember("validate$retainedBody").head.paramSymss.head.head.tree match
                  case ValDef(_, tt, _) =>
                    tt.tpe.asType match
                      case '[u] =>
                        val baseTypeRef = ReflectOnType.apply(ctx.quotes)(tt.tpe)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                        genWriteVal[u](ctx, cfg, '{ $aE.asInstanceOf[u] }, baseTypeRef.asInstanceOf[RTypeRef[u]], out, inTuple = inTuple)

              case t: AnyRef =>
                '{ AnyWriter.writeAny(${ Expr(cfg) }, $aE, $out) }

              case t: SelfRefRef[?] =>
                val sym = ctx.writeMethodSyms.getOrElse(
                  t.typedName,
                  throw new TypeError(s"Missing writer symbol for SelfRef: ${t.typedName}")
                )
                Apply(
                  Ref(sym),
                  List(aE.asTerm, out.asTerm)
                ).asExprOf[Unit]

              // Everything else...
              // case _ if isStringified => throw new JsonIllegalKeyType("Non-primitive/non-simple types cannot be map keys")
              case _ =>
                genEncFnBody(ctx, cfg, ref, aE, out, '{ false }, inTuple = inTuple, isMapKey = isMapKey)
        }
      )
