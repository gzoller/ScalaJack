package co.blocke.scalajack
package xml
package reading

import scala.quoted.*
import scala.util.{Failure, Success}
import scala.collection.Factory
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*
import shared.CodecBuildContext
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scalajack

object Reader:

  def genReadVal[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      ref: RTypeRef[T],
      in: Expr[XmlSource],
      inTuple: Boolean = false,
      isMapKey: Boolean = false,
      parentField: Option[FieldInfoRef],
      entryLabel: Option[String] = None,
      isStruct: Boolean = false
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // ------------------< Helpers
    given ToExpr[InputMode] with
      def apply(x: InputMode)(using Quotes): Expr[InputMode] = x match
        case InputMode.NORMAL => '{ InputMode.NORMAL }
        case InputMode.NAKED  => '{ InputMode.NAKED }
        case InputMode.STRUCT => '{ InputMode.STRUCT }

    def makeReadFnSym[S: Type](
        methodKey: TypedName
    ): Unit =
      val _ = ctx.readMethodSyms.getOrElseUpdate(
        methodKey,
        Symbol.newMethod(
          Symbol.spliceOwner,
          "r" + ctx.readMethodSyms.size,
          MethodType(List("in"))(_ => List(TypeRepr.of[XmlSource]), _ => TypeRepr.of[S])
        )
      )

    def makeReadFn(
        methodKey: TypedName,
        ref: RTypeRef[T]
    ): Expr[T] =
      Expr.summon[XmlCodec[T]] match {
        case Some(userOverride) => '{ ${ userOverride }.decodeValue($in) }
        case None =>
          ref match
            case t: Sealable if t.isSealed && t.childrenAreObject =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[XmlSource]
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
                    genReadVal[c](ctx, cfg, child.asInstanceOf[RTypeRef[c]], in, inTuple, isMapKey, parentField)
              }
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[XmlSource]
                Helpers.generateReaderBodyForSealedTraits[T](
                  ctx,
                  cfg,
                  t,
                  inExpr,
                  parentField,
                  isStruct
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: ScalaClassRef[?] =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[XmlSource]
                Helpers.generateReaderBodyForScalaClass[T](
                  ctx,
                  cfg,
                  methodKey,
                  t,
                  inExpr,
                  parentField,
                  isStruct
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: JavaClassRef[?] =>
              makeReadFnSym[T](methodKey)
              val bodyExprMaker: Tree => Expr[T] = { (inParam: Tree) =>
                val inExpr = Ref(inParam.symbol).asExprOf[XmlSource]
                Helpers.generateReaderBodyForJavaClass[T](
                  ctx,
                  cfg,
                  methodKey,
                  t,
                  inExpr,
                  parentField,
                  isStruct
                )
              }
              registerReaderDef(methodKey, bodyExprMaker)

            case t: TraitRef[?] =>
              throw UnsupportedType("Non-sealed traits are not supported")

            case t => // Should Never Happen(tm)
              throw UnsupportedType("Unsupported type: " + t.name)
      }

    def registerReaderDef(
        methodKey: TypedName,
        readerBodyExpr: Tree => Expr[T]
    ): Expr[T] =
      val readMethodSym = ctx.readMethodSyms.getOrElse(methodKey, throw new TypeError(s"Missing read fn symbol for $methodKey"))
      ctx.readMethodDefs(methodKey) = DefDef(
        readMethodSym,
        {
          case List(List(inParam)) =>
            val body = readerBodyExpr(inParam)
            Some(body.asTerm.changeOwner(readMethodSym))
          case other =>
            throw new TypeError(s"Macro failure. Unexpected parameter structure in DefDef: $other")
        }
      )

      // Always apply the function
      Apply(Ref(readMethodSym), List(in.asTerm)).asExprOf[T]

    def parseSimpleType(typeName: Expr[String], parse: Expr[String] => Expr[T], canBeNull: Expr[Boolean] = Expr(true)): Expr[T] =
      '{
        val raw = $in.expectSimpleValue()
        raw match
          case Some("null") =>
            if $canBeNull then null.asInstanceOf[T]
            else throw new ParseError($typeName + " not allowed to have a null value")
          case Some(s) if $canBeNull && s == "" =>
            throw new ParseError($typeName + " does not allow empty \"\" values")
          case Some(s) =>
            try ${ parse('s) }
            catch
              case e: Exception =>
                throw new ParseError("Can't parse " + $typeName + ": '" + s + "'")
          case None =>
            throw new ParseError("Expected a value for " + $typeName + " but found nothing")
      }.asExprOf[T]

    def getMode(rtypeRef: RTypeRef[?]): (Expr[InputMode], Expr[List[String]]) =
      val f = parentField.getOrElse(throw new ParseError("Required parent field not supplied for SeqRef"))
      (entryLabel, isStruct) match {
        case (None, false) =>
          rtypeRef match {
            case c: ClassRef[?] =>
              val elementLabel =
                c.annotations
                  .get("co.blocke.scalajack.xmlLabel")
                  .flatMap(_.get("name"))
                  .orElse(
                    f.annotations
                      .get("co.blocke.scalajack.xmlLabel")
                      .flatMap(_.get("name"))
                  )
                  .getOrElse(lastPart(c.name))
              (Expr(InputMode.NAKED), Expr(List(elementLabel)))
            case c: Sealable =>
//              val elementLabel =
//                c.annotations
//                  .get("co.blocke.scalajack.xmlLabel")
//                  .flatMap(_.get("name"))
//                  .orElse(
//                    f.annotations
//                      .get("co.blocke.scalajack.xmlLabel")
//                      .flatMap(_.get("name"))
//                  )
//                  .getOrElse(lastPart(c.name))
              val labels = c.sealedChildren.map(child => Helpers.sealedChildName(child.asInstanceOf[Sealable], parentField): String)
              (Expr(InputMode.NAKED), Expr(labels)) // GWZ
            case c: SelfRefRef[?] =>
              c.refType match {
                case '[f] =>
                  ReflectOnType[f](ctx.quotes)(TypeRepr.of[f], true)(using ctx.seenBefore) match {
                    case d: ClassRef[?] =>
                      val elementLabel =
                        d.annotations
                          .get("co.blocke.scalajack.xmlLabel")
                          .flatMap(_.get("name"))
                          .orElse(
                            f.annotations
                              .get("co.blocke.scalajack.xmlLabel")
                              .flatMap(_.get("name"))
                          )
                          .getOrElse(lastPart(c.name))
                      (Expr(InputMode.NAKED), Expr(List(elementLabel)))
                    case d: Sealable =>
//                      val elementLabel =
//                        d.annotations
//                          .get("co.blocke.scalajack.xmlLabel")
//                          .flatMap(_.get("name"))
//                          .orElse(
//                            f.annotations
//                              .get("co.blocke.scalajack.xmlLabel")
//                              .flatMap(_.get("name"))
//                          )
//                          .getOrElse(lastPart(c.name))
                      val labels = d.sealedChildren.map(child => Helpers.sealedChildName(child.asInstanceOf[Sealable], parentField): String)
                      (Expr(InputMode.NAKED), Expr(labels))
                  }
                case x =>
                  println("THERE: " + x.getClass.getName)
                  throw new ParseError(s"Field ${f.name} is a primitive value--requires @xmlEntryLabel")
              }
            case x =>
              println("HERE: " + x.getClass.getName)
              throw new ParseError(s"Field ${f.name} is a primitive value--requires @xmlEntryLabel")
          }
        case (Some(e), false) => (Expr(InputMode.NORMAL), Expr(List(e)))
        case (_, true) =>
          rtypeRef match {
            case c: ClassRef[?] =>
              val elementLabel =
                c.annotations
                  .get("co.blocke.scalajack.xmlLabel")
                  .flatMap(_.get("name"))
                  .orElse(
                    f.annotations
                      .get("co.blocke.scalajack.xmlLabel")
                      .flatMap(_.get("name"))
                  )
                  .getOrElse(lastPart(c.name))
              (Expr(InputMode.STRUCT), Expr(List(elementLabel)))
            case _ => throw new ParseError(s"Field ${f.name} marked with @xmlStruct that is not a class type--requires @xmlEntryLabel")
          }
      }

    // ---------------------------

    val methodKey = ref.typedName
    ref match

      // First cover all primitive and simple types...
      case t: BigDecimalRef => parseSimpleType(Expr("BigDecimal (Scala)"), s => '{ scala.math.BigDecimal($s) })
      case t: BigIntRef     => parseSimpleType(Expr("BigInt (Scala)"), s => '{ scala.math.BigInt($s) })
      case t: BooleanRef    => parseSimpleType(Expr("Boolean"), s => '{ $s.toBoolean }, Expr(false))
      case t: ByteRef       => parseSimpleType(Expr("Byte"), s => '{ $s.toByte }, Expr(false))
      case t: CharRef       => parseSimpleType(Expr("Char"), s => '{ $s.charAt(0) }, Expr(false))
      case t: DoubleRef     => parseSimpleType(Expr("Double"), s => '{ $s.toDouble }, Expr(false))
      case t: FloatRef      => parseSimpleType(Expr("Float"), s => '{ $s.toFloat }, Expr(false))
      case t: IntRef        => parseSimpleType(Expr("Int"), s => '{ $s.toInt }, Expr(false))
      case t: LongRef       => parseSimpleType(Expr("Long"), s => '{ $s.toLong }, Expr(false))
      case t: ShortRef      => parseSimpleType(Expr("Short"), s => '{ $s.toShort }, Expr(false))
      case t: StringRef =>
        '{
          $in.expectSimpleValue().getOrElse("") match {
            case "null" => null.asInstanceOf[T]
            case s      => s
          }
        }.asExprOf[T]

      case t: JBigDecimalRef => parseSimpleType(Expr("BigDecimal (Java)"), s => '{ new java.math.BigDecimal($s) })
      case t: JBigIntegerRef => parseSimpleType(Expr("BigInteger (Java)"), s => '{ new java.math.BigInteger($s) })
      case t: JBooleanRef    => parseSimpleType(Expr("Boolean (Java)"), s => '{ java.lang.Boolean.valueOf($s) })
      case t: JByteRef       => parseSimpleType(Expr("Byte (Java)"), s => '{ java.lang.Byte.valueOf($s) })
      case t: JCharacterRef =>
        parseSimpleType(
          Expr("Char (Java)"),
          s =>
            '{
              if $s == "" then throw new ParseError("Char (Java) does not allow empty \"\" values")
              else java.lang.Character.valueOf($s.charAt(0))
            }
        )
      case t: JDoubleRef  => parseSimpleType(Expr("Double (Java)"), s => '{ java.lang.Double.valueOf($s) })
      case t: JFloatRef   => parseSimpleType(Expr("Float (Java)"), s => '{ java.lang.Float.valueOf($s) })
      case t: JIntegerRef => parseSimpleType(Expr("Integer (Java)"), s => '{ java.lang.Integer.valueOf($s) })
      case t: JLongRef    => parseSimpleType(Expr("Long (Java)"), s => '{ java.lang.Long.valueOf($s) })
      case t: JShortRef   => parseSimpleType(Expr("Short (Java)"), s => '{ java.lang.Short.valueOf($s) })
      case t: JNumberRef =>
        parseSimpleType(
          Expr("Number (Java)"),
          s =>
            '{
              scala.math.BigDecimal($s) match {
                case d if d.isValidByte     => java.lang.Byte.valueOf(d.toByteExact)
                case d if d.isValidShort    => java.lang.Short.valueOf(d.toShortExact)
                case d if d.isValidInt      => java.lang.Integer.valueOf(d.toIntExact)
                case d if d.isValidLong     => java.lang.Long.valueOf(d.toLongExact)
                case d if d.isDecimalFloat  => java.lang.Float.valueOf(d.toFloat)
                case d if d.isDecimalDouble => java.lang.Double.valueOf(d.toDouble)
                case d                      => d
              }
            }
        )

      case t: DurationRef       => parseSimpleType(Expr("Duration"), s => '{ java.time.Duration.parse($s) })
      case t: InstantRef        => parseSimpleType(Expr("Instant"), s => '{ java.time.Instant.parse($s) })
      case t: LocalDateRef      => parseSimpleType(Expr("LocalDate"), s => '{ java.time.LocalDate.parse($s) })
      case t: LocalDateTimeRef  => parseSimpleType(Expr("LocalDateTime"), s => '{ java.time.LocalDateTime.parse($s) })
      case t: LocalTimeRef      => parseSimpleType(Expr("LocalTime"), s => '{ java.time.LocalTime.parse($s) })
      case t: MonthDayRef       => parseSimpleType(Expr("MonthDay"), s => '{ java.time.MonthDay.parse($s) })
      case t: OffsetDateTimeRef => parseSimpleType(Expr("OffsetDateTime"), s => '{ java.time.OffsetDateTime.parse($s) })
      case t: OffsetTimeRef     => parseSimpleType(Expr("OffsetTime"), s => '{ java.time.OffsetTime.parse($s) })
      case t: PeriodRef         => parseSimpleType(Expr("Period"), s => '{ java.time.Period.parse($s) })
      case t: YearRef           => parseSimpleType(Expr("Year"), s => '{ java.time.Year.parse($s) })
      case t: YearMonthRef      => parseSimpleType(Expr("YearMonth"), s => '{ java.time.YearMonth.parse($s) })
      case t: ZonedDateTimeRef  => parseSimpleType(Expr("ZonedDateTime"), s => '{ java.time.ZonedDateTime.parse($s) })
      case t: ZoneIdRef         => parseSimpleType(Expr("ZoneId"), s => '{ java.time.ZoneId.of($s) })
      case t: ZoneOffsetRef     => parseSimpleType(Expr("ZoneOffset"), s => '{ java.time.ZoneOffset.of($s) })

      case t: URLRef  => parseSimpleType(Expr("URL"), s => '{ new java.net.URI($s).toURL })
      case t: URIRef  => parseSimpleType(Expr("URI"), s => '{ new java.net.URI($s) })
      case t: UUIDRef => parseSimpleType(Expr("UUID"), s => '{ java.util.UUID.fromString($s) })

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
                  isMapKey,
                  parentField,
                  entryLabel
                )
              }.asInstanceOf[T]
            }

      // --------------------
      //  Options...
      // --------------------
      case t: ScalaOptionRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            if cfg.noneAsNull || inTuple then
              '{
                if $in.nextIsEmpty then None
                else Some(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in, inTuple, isMapKey, parentField, entryLabel).asExprOf[e] })
              }.asExprOf[T]
            else
              '{
                if $in.nextIsEmpty then null
                else ${ ofOption[e](Some(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in, inTuple, isMapKey, parentField, entryLabel).asExprOf[e])) }
              }.asExprOf[T]

      case t: JavaOptionalRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            if cfg.noneAsNull || inTuple then
              '{
                if $in.nextIsEmpty then java.util.Optional.empty
                else java.util.Optional.of(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in, inTuple, isMapKey, parentField, entryLabel).asExprOf[e] })
              }.asExprOf[T]
            else
              '{
                if $in.nextIsEmpty then null
                else ${ ofOptional[e](java.util.Optional.of(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in, inTuple, isMapKey, parentField, entryLabel).asExprOf[e])) }
              }.asExprOf[T]

      /*
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
        throw TypeError("Intersection types currently unsupported by ScalaJack")
       */

      // --------------------
      //  Enumerations...
      // --------------------
      /*
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
       */

      // --------------------
      //  Collections...
      // --------------------
      case t: SeqRef[?] =>
        ref.refType match
          case '[List[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val (modeE, entryLabelE) = getMode(rtypeRef)
//                println("           Seq gen for rtype " + rtypeRef.name + " isStruct? " + isStruct)
                '{
                  val parsedArray = $in.expectArray[e]($entryLabelE, () => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple, false, parentField, entryLabel, isStruct).asExprOf[e] }, $modeE)
                  if parsedArray != null then parsedArray.toList
                  else null
                }.asExprOf[T]
          case '[Vector[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val (modeE, entryLabelE) = getMode(rtypeRef)
                '{
                  val parsedArray = $in.expectArray[e]($entryLabelE, () => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple, false, parentField, entryLabel, isStruct).asExprOf[e] }, $modeE)
                  if parsedArray != null then parsedArray.toVector
                  else null
                }.asExprOf[T]
          case '[IndexedSeq[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val (modeE, entryLabelE) = getMode(rtypeRef)
                '{
                  val parsedArray = $in.expectArray[e]($entryLabelE, () => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple, false, parentField, entryLabel, isStruct).asExprOf[e] }, $modeE)
                  if parsedArray != null then parsedArray.toIndexedSeq
                  else null
                }.asExprOf[T]
          case '[Seq[?]] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val (modeE, entryLabelE) = getMode(rtypeRef)
                '{
                  val parsedArray = $in.expectArray[e]($entryLabelE, () => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple, false, parentField, entryLabel, isStruct).asExprOf[e] }, $modeE)
                  if parsedArray != null then parsedArray.toSeq
                  else null
                }.asExprOf[T]
          // Catch all, with (slightly) slower type coersion to proper Seq flavor
          case _ =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val (modeE, entryLabelE) = getMode(rtypeRef)
                '{
                  val parsedArray = $in.expectArray[e]($entryLabelE, () => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple, false, parentField, entryLabel, isStruct).asExprOf[e] }, $modeE)
                  if parsedArray != null then parsedArray.to(${ Expr.summon[Factory[e, T]].get }) // create appropriate flavor of Seq[T] here
                  else null
                }.asExprOf[T]

      /*
      case t: IterableRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
            '{
              val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple).asExprOf[e] })
              if parsedArray != null then parsedArray.asInstanceOf[Iterable[e]]
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
       */

      // --------------------
      //  Java Collections...
      // --------------------
      /*
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
                  case x => throw TypeError("Only primitive types supported for TreeSet, NavigableSet, or SortedSet. You've specified " + x.name)
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
       */

      // --------------------
      //  Maps...
      // --------------------
      /*
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
       */

      // --------------------
      //  Java Maps...
      // --------------------
      /*
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
       */

      // --------------------
      //  Tuples...
      // --------------------
      /*
      case t: TupleRef[?] =>
        import quotes.reflect.*
        t.refType match
          case '[tt] =>
            val tpe = TypeRepr.of[tt]
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
       */

      // --------------------
      //  Try...
      // --------------------
      /*
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
       */

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
                val arg = genReadVal[f](ctx, cfg, theField.asInstanceOf[RTypeRef[f]], in, inTuple, isMapKey, parentField, entryLabel, isStruct).asTerm
                Apply(constructor, List(arg)).asExprOf[T]

      /*
      // --------------------
      //  NeoType...
      // --------------------
      case t: NeoTypeRef[?] => // in Quotes context
        val module = Symbol.requiredModule(t.typedName.toString)
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
       */

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
          throw new TypeError(s"Missing reader method symbol for $methodKey")
        )
        Apply(Ref(readerSym), List(in.asTerm)).asExprOf[T]

      case t: AnyRef =>
        ctx.seenAnyRef = true
        Ref(ctx.readAnySym).appliedTo(in.asTerm).asExprOf[T]

      case t => throw new ParseError("Not yet implemented: " + t)
