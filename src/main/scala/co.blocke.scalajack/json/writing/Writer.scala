package co.blocke.scalajack
package json
package writing

import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RType, RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.rtypes.EnumRType

object Writer:

  // Fantastic Dark Magic here--lifted from Jasoniter.  Props!  This thing will create a DefDef, and a Symbol to it.
  // The Symbol will let you call the generated function later from other macro-generated code.  The goal is to use
  // generated functions to create cleaner/faster macro code than what straight quotes/splices would create unaided.
  private def makeWriteFn[U: Type](
      ctx: CodecBuildContext,
      methodKey: TypedName,
      arg: Expr[U],
      out: Expr[JsonOutput]
  )(f: (Expr[U], Expr[JsonOutput]) => Expr[Unit]): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val sym =
      ctx.writeMethodSyms.getOrElseUpdate(
        methodKey, {
          val newSym = Symbol.newMethod(
            Symbol.spliceOwner,
            "w" + ctx.writeMethodSyms.size,
            MethodType(List("in", "out"))(
              _ => List(TypeRepr.of[U], TypeRepr.of[JsonOutput]),
              _ => TypeRepr.of[Unit]
            )
          )
          ctx.writeMethodDefs.update(
            methodKey,
            DefDef(
              newSym,
              params => {
                val List(List(in, out)) = params
                Some(f(in.asExprOf[U], out.asExprOf[JsonOutput]).asTerm.changeOwner(newSym))
              }
            )
          )
          newSym
        }
      )

    // Populate writerMap for SelfRef support
    ctx.writerFnMapEntries(methodKey) = '{ (in: Any, out: JsonOutput) =>
      ${
        Apply(Ref(sym), List('{ in.asInstanceOf[U] }.asTerm, 'out.asTerm)).asExprOf[Unit]
      }
    }
//    ctx.writerFnMapEntries(methodKey) = '{ (in: Any, out: JsonOutput) =>
//      ${ Apply(Ref(sym), List('in.asTerm, 'out.asTerm)).asExprOf[Unit] }
//    }

    // Always apply the function
    Apply(Ref(sym), List(arg.asTerm, out.asTerm)).asExprOf[Unit]

  // ---------------------------------------------------------------------------------------------

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
  // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
  // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
  //
  // TODO: This is a stub. Implement real one after SelfRef function works
  //
  private def maybeWrite[T](ctx: CodecBuildContext, cfg: SJConfig, label: String, aE: Expr[T], ref: RTypeRef[T], out: Expr[JsonOutput]): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val labelE = Expr(label)
    val prefix = '{ $out.label($labelE) }
    ref.refType match
      case '[u] =>
        '{
          $prefix
          ${ genWriteVal[u](ctx, cfg, aE.asExprOf[u], ref.asInstanceOf[RTypeRef[u]], out) }
        }

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

          case t: SeqRef[?] =>
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              t.elementRef.refType match
                case '[e] =>
                  val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Seq[e]] else in.asExprOf[Seq[e]]
                  '{
                    if $tin == null then $out.burpNull()
                    else
                      $out.startArray()
                      $tin.foreach { i =>
                        ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                      }
                      $out.endArray()
                  }
            }

          // Basically sealed traits...
          case t: Sealable if t.isSealed =>
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              if t.childrenAreObject then
                '{
                  if $in == null then $out.burpNull()
                  else $out.value($in.getClass.getName.split('.').last.stripSuffix("$"))
                }
              else
                val unique = Unique.findUniqueWithExcluded(t)
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
                        genEncFnBody[c](ctx, cfg, child, Ref(sym).asExprOf[c], out, renderHint).asTerm
                      )
                } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                Match(in.asTerm, cases).asExprOf[Unit]
            }

          case t: ScalaClassRef[?] =>
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              val body = {
                val eachField = t.fields.map { f =>
                  f.fieldRef.refType match
                    case '[z] =>
                      val fieldValue = Select.unique(in.asTerm, f.name).asExprOf[z]
                      val fieldName = changeFieldName(f)
                      maybeWrite[z](ctx, cfg, fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out)
                }
                // ZZZ -- To block.... (soak this to see if it works then delete old block
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
                /* ZZZ
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
                 */
              }

              if !t.isCaseClass && cfg._writeNonConstructorFields then
                val eachField = t.nonConstructorFields.map { f =>
                  f.fieldRef.refType match
                    case '[e] =>
                      val fieldValue = Select.unique(in.asTerm, f.getterLabel).asExprOf[e]
                      val fieldName = changeFieldName(f)
                      maybeWrite[e](ctx, cfg, fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[e]], out)
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

          case t => throw new JsonUnsupportedType("Type represented by " + t.name + " is unsupported for JSON writes")

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
//    val cacheHit = ref match {
//      case sr: SelfRefRef[?] => None
//      case _ =>
//        ctx.writeMethodSyms
//          .get(methodKey)
//          .map { sym => // hit cache first... then match on Ref type
//            Apply(Ref(sym), List(aE.asTerm, out.asTerm)).asExprOf[Unit]
//          }
//    }
//    cacheHit
//      .getOrElse(
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

      // NeoType is a bit of a puzzle-box.  To get the correct underlying base type, I had to dig into
      // the argument of method validate$retainedBody.  It happened to have the correctly-typed parameter.
      // With the correct type, we can correct write out the value.
      case t: NeoTypeRef[?] => // in Quotes context
        Symbol.requiredModule(t.typedName.toString).methodMember("validate$retainedBody").head.paramSymss.head.head.tree match
          case ValDef(_, tt, _) =>
            tt.tpe.asType match
              case '[u] =>
                val baseTypeRef = ReflectOnType.apply(ctx.quotes)(tt.tpe)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                genWriteVal[u](ctx, cfg, '{ $aE.asInstanceOf[u] }, baseTypeRef.asInstanceOf[RTypeRef[u]], out)

      case t: AnyRef => '{ AnyWriter.writeAny($aE, $out, ${ Expr(cfg) }) }

      case t: SelfRefRef[?] =>
        println(s"<<< SelfRefRef found: $methodKey >>>")
        val mapExpr = Ref(ctx.writerMapSym).asExprOf[Map[String, (Any, JsonOutput) => Unit]]
        val keyExpr = Expr(t.typedName.toString)
        '{
          $mapExpr($keyExpr).apply($aE, $out)
        }
//      case t: SelfRefRef[?] =>
//        println(s"<<< SelfRefRef found: $methodKey >>>")
//        val mapExpr = Ref(ctx.writerMapSym).asExprOf[Map[String, (Any, JsonOutput) => Unit]] // Symbol for `val readerMap = Map(...)`
//        val keyExpr = Expr(t.typedName.toString)
//        '{
//          $mapExpr($keyExpr).apply($aE, $out).asInstanceOf[T]
//        }
//        '{
//          val fn = $mapExpr($keyExpr)
//          fn($aE.asInstanceOf[Any], $out)
//        }

      // Everything else...
      // case _ if isStringified => throw new JsonIllegalKeyType("Non-primitive/non-simple types cannot be map keys")
      case _ => genEncFnBody(ctx, cfg, ref, aE, out, '{ false }, inTuple = inTuple, isMapKey = isMapKey)
//  )
