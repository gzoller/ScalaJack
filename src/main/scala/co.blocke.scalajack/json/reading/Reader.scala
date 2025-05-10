package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import scala.util.{Failure, Success}
import scala.collection.Factory
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}

object Reader:

  def genReadVal[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      ref: RTypeRef[T],
      in: Expr[JsonSource],
      inTuple: Boolean = false,
      isMapKey: Boolean = false
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // ------------------< Helpers

    def makeReadFnSym[S: Type](
        methodKey: TypedName
    ): Unit =
      ctx.readMethodSyms.getOrElseUpdate(
        methodKey,
        Symbol.newMethod(
          Symbol.spliceOwner,
          "r" + ctx.readMethodSyms.size,
          MethodType(List("in"))(_ => List(TypeRepr.of[JsonSource]), _ => TypeRepr.of[S])
        )
      )

    def makeReadFn(
        methodKey: TypedName,
        ref: RTypeRef[T]
    ): Expr[T] =
      Expr.summon[JsonCodec[T]] match {
        case Some(userOverride) => '{ ${ userOverride }.decodeValue($in) }
        case None =>
          ref match
            case t: Sealable if t.isSealed && t.childrenAreObject =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[JsonSource]
                Helpers.generateReaderBodyForCaseObjects[T](
                  ctx,
                  t.sealedChildren,
                  t.name,
                  inExpr
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: Sealable if t.isSealed && !t.childrenAreObject =>
              makeReadFnSym[T](methodKey)
              t.sealedChildren.foreach { child =>
                child.refType match
                  case '[c] =>
                    genReadVal[c](ctx, cfg, child.asInstanceOf[RTypeRef[c]], in, inTuple, isMapKey)
              }
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[JsonSource]
                Helpers.generateReaderBodyForSealedTraits[T](
                  ctx,
                  cfg,
                  t,
                  inExpr
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: ScalaClassRef[?] =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[JsonSource]
                Helpers.generateReaderBodyForScalaClass[T](
                  ctx,
                  cfg,
                  methodKey,
                  t,
                  inExpr
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: JavaClassRef[?] =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[JsonSource]
                Helpers.generateReaderBodyForJavaClass[T](
                  ctx,
                  cfg,
                  methodKey,
                  t,
                  inExpr
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: TraitRef[?] =>
              throw JsonUnsupportedType("Non-sealed traits are not supported")

            case t => // Should Never Happen(tm)
              throw JsonUnsupportedType("Unsupported type: " + t.name)
      }

    def registerReaderDef(
        methodKey: TypedName,
        readerBodyExpr: Tree => Expr[T]
    ): Expr[T] =
      val readMethodSym = ctx.readMethodSyms.getOrElse(methodKey, throw new JsonTypeError(s"Missing read fn symbol for $methodKey"))
      ctx.readMethodDefs(methodKey) = DefDef(
        readMethodSym,
        {
          case List(List(inParam)) =>
            val body = readerBodyExpr(inParam)
            Some(body.asTerm.changeOwner(readMethodSym))
          case other =>
            throw new JsonTypeError(s"Macro failure. Unexpected parameter structure in DefDef: $other")
        }
      )

      // Always apply the function
      Apply(Ref(readMethodSym), List(in.asTerm)).asExprOf[T]

    // ---------------------------

    val methodKey = ref.typedName
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

      case t: DurationRef       => '{ $in.expectStringWithFn(java.time.Duration.parse) }.asExprOf[T]
      case t: InstantRef        => '{ $in.expectStringWithFn(java.time.Instant.parse) }.asExprOf[T]
      case t: LocalDateRef      => '{ $in.expectStringWithFn(java.time.LocalDate.parse) }.asExprOf[T]
      case t: LocalDateTimeRef  => '{ $in.expectStringWithFn(java.time.LocalDateTime.parse) }.asExprOf[T]
      case t: LocalTimeRef      => '{ $in.expectStringWithFn(java.time.LocalTime.parse) }.asExprOf[T]
      case t: MonthDayRef       => '{ $in.expectStringWithFn(java.time.MonthDay.parse) }.asExprOf[T]
      case t: OffsetDateTimeRef => '{ $in.expectStringWithFn(java.time.OffsetDateTime.parse) }.asExprOf[T]
      case t: OffsetTimeRef     => '{ $in.expectStringWithFn(java.time.OffsetTime.parse) }.asExprOf[T]
      case t: PeriodRef         => '{ $in.expectStringWithFn(java.time.Period.parse) }.asExprOf[T]
      case t: YearRef           => '{ $in.expectStringWithFn(java.time.Year.parse) }.asExprOf[T]
      case t: YearMonthRef      => '{ $in.expectStringWithFn(java.time.YearMonth.parse) }.asExprOf[T]
      case t: ZonedDateTimeRef  => '{ $in.expectStringWithFn(java.time.ZonedDateTime.parse) }.asExprOf[T]
      case t: ZoneIdRef         => '{ $in.expectStringWithFn(java.time.ZoneId.of) }.asExprOf[T]
      case t: ZoneOffsetRef     => '{ $in.expectStringWithFn(java.time.ZoneOffset.of) }.asExprOf[T]

      case t: URLRef  => '{ $in.expectStringWithFn((s: String) => new java.net.URI(s).toURL) }.asExprOf[T]
      case t: URIRef  => '{ $in.expectStringWithFn((s: String) => new java.net.URI(s)) }.asExprOf[T]
      case t: UUIDRef => '{ $in.expectStringWithFn(java.util.UUID.fromString) }.asExprOf[T]

      case t: AliasRef[?] =>
        t.unwrappedType.refType match
          case '[e] =>
            '{
              ${
                genReadVal[e](
                  ctx,
                  cfg,
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
      case t: ScalaOptionRef[?] =>
        import quotes.reflect.*
        t.optionParamType.refType match
          case '[e] =>
            if cfg.noneAsNull || inTuple then
              '{
                if $in.expectNull() then None
                else Some(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in).asExprOf[e] })
              }.asExprOf[T]
            else
              '{
                if $in.expectNull() then null
                else ${ ofOption[e](Some(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in).asExprOf[e])) }
              }.asExprOf[T]

      case t: JavaOptionalRef[?] =>
        import quotes.reflect.*
        t.optionParamType.refType match
          case '[e] =>
            if cfg.noneAsNull || inTuple then
              '{
                if $in.expectNull() then java.util.Optional.empty
                else java.util.Optional.of(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in).asExprOf[e] })
              }.asExprOf[T]
            else
              '{
                if $in.expectNull() then null
                else ${ ofOptional[e](java.util.Optional.of(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in).asExprOf[e])) }
              }.asExprOf[T]

      case t: LeftRightRef[?] if t.lrkind == LRKind.EITHER =>
        import quotes.reflect.*
        t.leftRef.refType match
          case '[l] =>
            t.rightRef.refType match
              case '[r] =>
                '{
                  val mark = $in.pos
                  if $in.expectNull() then null
                  else
                    scala.util.Try(
                      ${
                        t.rightRef match
                          case rRef: RTypeRef[`r`] =>
                            genReadVal[`r`](ctx, cfg, rRef, in, inTuple)
                          case null =>
                            report.errorAndAbort("Unexpected type for rightRef in Either")
                      }
                    ) match
                      case Success(rval) =>
                        Right(rval)
                      case Failure(_) =>
                        $in.revertToPos(mark)
                        scala.util.Try(
                          ${
                            t.leftRef match
                              case lRef: RTypeRef[`l`] =>
                                genReadVal[`l`](ctx, cfg, lRef, in, inTuple)
                              case null =>
                                report.errorAndAbort("Unexpected type for leftRef in Either")
                          }
                        ) match
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
                  val mark = $in.pos
                  scala.util.Try(
                    ${
                      t.leftRef match
                        case lRef: RTypeRef[`l`] =>
                          genReadVal[`l`](ctx, cfg, lRef, in, true)
                        case null =>
                          report.errorAndAbort("Unexpected type for leftRef")
                    }
                  ) match
                    case Success(lval) => lval
                    case Failure(_) =>
                      $in.revertToPos(mark)
                      scala.util.Try(
                        ${
                          t.rightRef match
                            case rRef: RTypeRef[`r`] =>
                              genReadVal[`r`](ctx, cfg, rRef, in, true)
                            case null =>
                              report.errorAndAbort("Unexpected type for rightRef")
                        }
                      ) match
                        case Success(rval) => rval
                        case Failure(_) =>
                          $in.backspace()
                          throw JsonParseError("Failed to read either side of Union type", $in)
                }.asExprOf[T]

      case t: LeftRightRef[?] if t.lrkind == LRKind.INTERSECTION =>
        throw JsonTypeError("Intersection types currently unsupported by ScalaJack")

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
                    cfg.enumsAsIds match
                      case List("-") =>
                        val typeRepr = TypeRepr.of[e]
                        val m = typeRepr.typeSymbol.companionClass.declaredMethod("valueOf")
                        Apply(Ref(m(0)), List('{ s }.asTerm)).asExpr
                      case c if c == Nil || c.contains(t.name) =>
                        val typeRepr = TypeRepr.of[e]
                        val m = typeRepr.typeSymbol.companionClass.declaredMethod("fromOrdinal")
                        Apply(Ref(m(0)), List('{ s.toInt }.asTerm)).asExpr
                      case _ =>
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
                    cfg.enumsAsIds match
                      case List("-") =>
                        val enumeration = TypeRepr.of[e] match
                          case TypeRef(ct, _) => Ref(ct.termSymbol).asExprOf[Enumeration]
                        '{ ${ enumeration }.values.iterator.find(_.toString == s).get }.asExprOf[e]
                      case c if c == Nil || c.contains(t.name) =>
                        val enumeration = TypeRepr.of[e] match
                          case TypeRef(ct, _) => Ref(ct.termSymbol).asExprOf[Enumeration]
                        '{ ${ enumeration }.values.iterator.find(_.id == s.toInt).get }.asExprOf[e]
                      case _ =>
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
            val valuesE = Expr(t.values)
            '{
              $in.expectEnum() match
                case null => null
                case s: String =>
                  scala.util.Try(s.toInt) match
                    case Success(v) =>
                      ${
                        val typeRepr = TypeRepr.of[e]
                        val m = typeRepr.classSymbol.get.companionModule.methodMember("valueOf")
                        Apply(Ref(m(0)), List('{ $valuesE(v) }.asTerm)).asExpr
                      }
                    case _ =>
                      ${
                        val typeRepr = TypeRepr.of[e]
                        val m = typeRepr.classSymbol.get.companionModule.methodMember("valueOf")
                        Apply(Ref(m(0)), List('{ s }.asTerm)).asExpr
                      }
                case v: Int =>
                  ${
                    val typeRepr = TypeRepr.of[e]
                    val m = typeRepr.classSymbol.get.companionModule.methodMember("valueOf")
                    Apply(Ref(m(0)), List('{ $valuesE(v) }.asTerm)).asExpr
                  }
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
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then parsedArray.toList
                  else null
                }.asExprOf[T]
          case '[Vector[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then parsedArray.toVector
                  else null
                }.asExprOf[T]
          case '[IndexedSeq[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then parsedArray.toIndexedSeq
                  else null
                }.asExprOf[T]
          case '[Seq[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then parsedArray.toSeq
                  else null
                }.asExprOf[T]
          // Catch all, with (slightly) slower type coersion to proper Seq flavor
          case _ =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then parsedArray.to(${ Expr.summon[Factory[e, T]].get }) // create appropriate flavor of Seq[T] here
                  else null
                }.asExprOf[T]

      case t: IterableRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
            val ct = Expr.summon[ClassTag[e]].get
            '{
              val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
              if parsedArray != null then
                implicit val ctt = $ct
                parsedArray.asInstanceOf[Iterable[e]]
              else null
            }.asExprOf[T]

      case t: ArrayRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
            val ct = Expr.summon[ClassTag[e]].get
            '{
              val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
              if parsedArray != null then
                implicit val ctt = $ct
                parsedArray.toArray[e]
              else null
            }.asExprOf[T]

      case t: SetRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
            '{
              val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
              if parsedArray != null then parsedArray.to(${ Expr.summon[Factory[e, T]].get }) // create appropriate flavor of Set[T] here
              else null
            }.asExprOf[T]

      // --------------------
      //  Java Collections...
      // --------------------
      case t: JavaCollectionRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
            ref.name match
              // Non-standard concrete Java Collecitons
              case "java.util.concurrent.ArrayBlockingQueue" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray == null then null
                  else new java.util.concurrent.ArrayBlockingQueue(parsedArray.length, true, parsedArray.toList.asJava)
                }.asExprOf[T]
              // java.util.Collection interfaces
              case "java.util.Stack" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray == null then null
                  else
                    val s = new java.util.Stack[e]()
                    parsedArray.map(j => s.push(j))
                    s
                }.asExprOf[T]
              case "java.util.concurrent.TransferQueue" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then new java.util.concurrent.LinkedTransferQueue(parsedArray.toList.asJava)
                  else null
                }.asExprOf[T]
              case "java.util.concurrent.BlockingQueue" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then new java.util.concurrent.LinkedBlockingQueue(parsedArray.toList.asJava)
                  else null
                }.asExprOf[T]
              case "java.util.TreeSet" | "java.util.NavigableSet" | "java.util.SortedSet" =>
                t.elementRef match
                  case _: PrimitiveRef =>
                    t.elementRef.refType match
                      case '[z] =>
                        '{
                          val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                          if parsedArray != null then new java.util.TreeSet(parsedArray.toList.asJava)
                          else null
                        }.asExprOf[T]
                  case x => throw JsonTypeError("Only primitive types supported for TreeSet, NavigableSet, or SortedSet. You've specified " + x.name)
              case "java.util.Queue" | "java.util.Deque" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then new java.util.LinkedList(parsedArray.toList.asJava)
                  else null
                }.asExprOf[T]
              case "java.util.Set" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then new java.util.HashSet(parsedArray.toList.asJava)
                  else null
                }.asExprOf[T]
              case "java.util.List" | "java.lang.Iterable" =>
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
                  if parsedArray != null then new java.util.ArrayList(parsedArray.toList.asJava)
                  else null
                }.asExprOf[T]
              // Normal concrete Java Collecitons
              case _ =>
                '{
                  if $in.expectNull() then null
                  else
                    ${
                      val arg = '{
                        val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[e]], in, inTuple).asExprOf[e] })
                        parsedArray.toList.asJava.asInstanceOf[java.util.Collection[e]]
                      }.asTerm
                      Select
                        .overloaded(
                          New(Inferred(TypeRepr.of[T])),
                          "<init>",
                          List(TypeRepr.of[e]),
                          List(arg)
                        )
                        .asExprOf[T]
                    }
                }.asExprOf[T]

      // --------------------
      //  Maps...
      // --------------------
      case t: MapRef[?] =>
        ref.refType match
          case '[scala.collection.mutable.LinkedHashMap[?, ?]] => // immutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.LinkedHashMap.from(
                          $in.parseOrderedMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            scala.collection.mutable.LinkedHashMap.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.mutable.ListMap[?, ?]] => // immutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.ListMap.from(
                          $in.parseOrderedMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            scala.collection.mutable.LinkedHashMap.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.mutable.HashMap[?, ?]] =>
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.HashMap.from(
                          $in.parseMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            Map.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.mutable.Map[?, ?]] if ref.name == "scala.collection.mutable.Map" =>
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.Map.from(
                          $in.parseMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            Map.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.mutable.SeqMap[?, ?]] => // mutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.SeqMap.from(
                          $in.parseOrderedMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            scala.collection.mutable.LinkedHashMap.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.mutable.Map[?, ?]] => // all other mutable Maps
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.mutable.Map
                          .from(
                            $in.parseMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              Map.empty[k, v],
                              true
                            )
                          )
                          .to(${ Expr.summon[Factory[(k, v), T]].get })
                    }.asExprOf[T]
          case '[scala.collection.immutable.ListMap[?, ?]] => // immutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.immutable.ListMap.from(
                          $in.parseOrderedMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            scala.collection.mutable.LinkedHashMap.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.immutable.HashMap[?, ?]] => // immutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.immutable.HashMap.from(
                          $in.parseMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            Map.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[scala.collection.immutable.SeqMap[?, ?]] => // immutable
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        scala.collection.immutable.SeqMap.from(
                          $in.parseOrderedMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            scala.collection.mutable.LinkedHashMap.empty[k, v],
                            true
                          )
                        )
                    }.asExprOf[T]
          case '[Map[?, ?]] if ref.name == "scala.collection.immutable.Map" => // immutable
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
                          () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                          () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                          Map.empty[k, v],
                          true
                        )
                    }.asExprOf[T]
          case '[Map[?, ?]] => // all other immutable Maps
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        $in
                          .parseMap[k, v](
                            () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                            () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                            Map.empty[k, v],
                            true
                          )
                          .to(${ Expr.summon[Factory[(k, v), T]].get })
                    }.asExprOf[T]

      // --------------------
      //  Java Maps...
      // --------------------
      case t: JavaMapRef[?] =>
        t.elementRef.refType match
          case '[k] =>
            t.elementRef2.refType match
              case '[v] =>
                ref.name match
                  case "java.util.NavigableMap" | "java.util.SortedMap" | "java.util.TreeMap" =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        new java.util.TreeMap(
                          $in
                            .parseMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              Map.empty[k, v],
                              true
                            )
                            .asJava
                        )
                    }.asExprOf[T]
                  case "java.util.concurrent.ConcurrentMap" =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        new java.util.concurrent.ConcurrentHashMap(
                          $in
                            .parseMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              Map.empty[k, v],
                              true
                            )
                            .asJava
                        )
                    }.asExprOf[T]
                  case "java.util.concurrent.ConcurrentNavigableMap" =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        new java.util.concurrent.ConcurrentSkipListMap(
                          $in
                            .parseMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              Map.empty[k, v],
                              true
                            )
                            .asJava
                        )
                    }.asExprOf[T]
                  case "java.util.LinkedHashMap" =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        new java.util.LinkedHashMap(
                          $in
                            .parseOrderedMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              scala.collection.mutable.LinkedHashMap.empty[k, v],
                              true
                            )
                            .asJava
                        )
                    }.asExprOf[T]
                  case "java.util.Map" =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        $in.expectToken('{')
                        new java.util.HashMap(
                          $in
                            .parseMap[k, v](
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                              Map.empty[k, v],
                              true
                            )
                            .asJava
                        )
                    }.asExprOf[T]
                  case _ =>
                    testValidMapKey(t.elementRef)
                    '{
                      if $in.expectNull() then null
                      else
                        ${
                          val arg = '{
                            $in.expectToken('{')
                            $in
                              .parseMap[k, v](
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true).asExprOf[k] },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple).asExprOf[v] },
                                Map.empty[k, v],
                                true
                              )
                              .asJava
                          }.asTerm
                          Select
                            .overloaded(
                              New(Inferred(TypeRepr.of[T])),
                              "<init>",
                              List(TypeRepr.of[k], TypeRepr.of[v]),
                              List(arg)
                            )
                            .asExprOf[T]
                        }
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
              t.tupleRefs.zipWithIndex.map { case (tpart, i) =>
                tpart.refType match
                  case '[e] =>
                    if i == 0 then genReadVal[e](ctx, cfg, tpart.asInstanceOf[RTypeRef[e]], in, true).asTerm
                    else
                      '{
                        $in.expectToken(',')
                        ${ genReadVal[e](ctx, cfg, tpart.asInstanceOf[RTypeRef[e]], in, true) }
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

      // --------------------
      //  Try...
      // --------------------
      case t: TryRef[?] =>
        t.tryRef.refType match
          case '[e] =>
            '{
              val mark = $in.pos
              try
                if $in.expectNull() then null
                else scala.util.Success(${ genReadVal[e](ctx, cfg, t.tryRef.asInstanceOf[RTypeRef[e]], in) })
              catch {
                case t: Throwable =>
                  $in.revertToPos(mark)
                  throw JsonParseError("Unsuccessful attempt to read Try type with failure: " + t.getMessage, $in)
              }
            }.asExprOf[T]

      // --------------------
      //  Value Class...
      // --------------------
      case t: ScalaClassRef[?] if t.isValueClass =>
        val theField = t.fields.head.fieldRef
        t.refType match
          case '[e] =>
            theField.refType match
              case '[f] =>
                val tpe = TypeRepr.of[e]
                val constructor = Select(New(Inferred(tpe)), tpe.classSymbol.get.primaryConstructor)
                val arg = genReadVal[f](ctx, cfg, theField.asInstanceOf[RTypeRef[f]], in, isMapKey = isMapKey).asTerm
                Apply(constructor, List(arg)).asExprOf[T]

      // --------------------
      //  NeoType...
      // --------------------
      case t: NeoTypeRef[?] => // in Quotes context
        val module = Symbol.requiredModule(t.typedName.toString)
        val myMake = module.methodMember("make").head
        val tm = Ref(module)
        t.wrappedTypeRef.refType match
          case '[e] =>
            val res = Apply(
              Select.unique(tm, "make"),
              List(
                genReadVal[e](ctx, cfg, t.wrappedTypeRef.asInstanceOf[RTypeRef[e]], in, isMapKey = isMapKey).asTerm
              )
            ).asExprOf[Either[String, T]]
            val tnameE = Expr(t.name)
            '{
              $res match
                case Right(r) => r
                case Left(m) =>
                  $in.backspace()
                  throw JsonParseError("NeoType validation for " + $tnameE + " failed", $in)
            }.asExprOf[T]

      // --------------------
      //  Classes 'n Traits (all get passed off to MakeReadFn.makeReadFn()
      // --------------------
      case t: Sealable if t.isSealed && t.childrenAreObject =>
        makeReadFn(methodKey, t)

      case t: Sealable if t.isSealed && !t.childrenAreObject =>
        makeReadFn(methodKey, t)

      case t: ScalaClassRef[?] =>
        makeReadFn(methodKey, t)

      case t: JavaClassRef[?] =>
        makeReadFn(methodKey, t)

      case t: SelfRefRef[?] =>
        val readerSym = ctx.readMethodSyms.getOrElse(
          methodKey,
          throw new JsonTypeError(s"Missing reader method symbol for $methodKey")
        )
        Apply(Ref(readerSym), List(in.asTerm)).asExprOf[T]

      case t: AnyRef =>
        ctx.seenAnyRef = true
        Ref(ctx.readAnySym).appliedTo(in.asTerm).asExprOf[T]

      case t => throw new ParseError("Not yet implemented: " + t)
