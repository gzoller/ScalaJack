package co.blocke.scalajack
package xml
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
          _ => List(TypeRepr.of[U], TypeRepr.of[XmlOutput]),
          _ => TypeRepr.of[Unit]
        )
      )
    )

  private def makeWriteFn[U: Type](
      ctx: CodecBuildContext,
      methodKey: TypedName,
      arg: Expr[U],
      out: Expr[XmlOutput]
  )(f: (Expr[U], Expr[XmlOutput]) => Expr[Unit]): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val writeMethodSym = ctx.writeMethodSyms.getOrElse(methodKey, throw new XmlTypeError(s"Missing write fn symbol for $methodKey"))
    ctx.writeMethodDefs.update(
      methodKey,
      DefDef(
        writeMethodSym,
        params => {
          val List(List(in, out)) = params
          Some(f(in.asExprOf[U], out.asExprOf[XmlOutput]).asTerm.changeOwner(writeMethodSym))
        }
      )
    )

    // Always apply the function
    Apply(Ref(writeMethodSym), List(arg.asTerm, out.asTerm)).asExprOf[Unit]

  // ---------------------------------------------------------------------------------------------

  //  Writing (encoding) XML is accomplished with two functions: genEncFnBody() and genWriteVal(). The former is the primary entry for writing some
  //  XML. As part of writing it is common to write a value, for example the elements of a list. genWriteVal() handles this tedium, including recursively
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
      out: Expr[XmlOutput],
      emitDiscriminator: Expr[Boolean], // caller must pass this default ==> '{ false },
      inTuple: Boolean = false,
      isMapKey: Boolean = false,
      isStruct: Boolean = false,
      parentField: Option[FieldInfoRef]
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
                  else $out.emitValue($in.getClass.getName.split('.').last.stripSuffix("$"))
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
                        genEncFnBody[c](ctx, cfg, child, Ref(sym).asExprOf[c], out, renderHint, inTuple = inTuple, isMapKey = isMapKey, parentField = parentField).asTerm
                      )
                } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                Match(in.asTerm, cases).asExprOf[Unit]
            }

          // We don't use makeWriteFn here because a value class is basically just a "box" around a simple type
          /*
          case t: ScalaClassRef[?] if t.isValueClass =>
            val theField = t.fields.head.fieldRef
            theField.refType match
              case '[e] =>
                val fieldValue = Select.unique(aE.asTerm, t.fields.head.name).asExprOf[e]
                genWriteVal(ctx, cfg, fieldValue, theField.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple, isMapKey = isMapKey)
           */

          case t: ScalaClassRef[?] =>
            makeWriteFnSymbol(ctx, methodKey)
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              val body = {
                val eachField = t.fields.map { f =>
                  f.fieldRef.refType match
                    case '[z] =>
                      val fieldValue = Select.unique(in.asTerm, f.name).asExprOf[z]
                      val entryLabel = f.annotations
                        .get("co.blocke.scalajack.xmlEntryLabel")
                        .flatMap(_.get("name"))
                      val isStruct = f.annotations.contains("co.blocke.scalajack.xmlStruct")
                      // entryLabel == None -> is naked
                      // isStruct == true -> is struct
                      // isStruct supersedes is naked
                      MaybeWrite.maybeWriteField[z](ctx, cfg, f, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out, entryLabel, isStruct)
                }
                val noDiscBlock: Expr[Unit] =
                  eachField match
                    case Nil      => '{ () }
                    case x :: Nil => x
                    case xs       => Expr.block(xs.init, xs.last)

                noDiscBlock.asTerm

                // Unlike JSON, we don't worry about type hints here. The actual type is emitted in the XML label for the object, so it becomes the hint!
              }.asExprOf[Unit]

              val className =
                if isStruct then
                  val n = t.annotations
                    .get("co.blocke.scalajack.xmlLabel")
                    .flatMap(_.get("name"))
                    .orElse(parentField.flatMap(_.annotations.get("co.blocke.scalajack.xmlLabel").flatMap(_.get("name"))))
                    .getOrElse(lastPart(t.name))
//                  println(s"Class ${t.name} resolved: $n")
                  Expr(
                    n
                  )
                else
                  val n = t.annotations
                    .get("co.blocke.scalajack.xmlLabel")
                    .flatMap(_.get("name"))
                    .getOrElse(lastPart(t.name))
//                  println(s"Non-Struct Class ${t.name} resolved: $n")
                  Expr(
                    n
                  )

              if !t.isCaseClass && cfg._writeNonConstructorFields then
                val eachField = t.nonConstructorFields.map { f =>
                  f.fieldRef.refType match
                    case '[e] =>
                      val fieldValue = Select.unique(in.asTerm, f.getterLabel).asExprOf[e]
                      MaybeWrite.maybeWriteField[e](ctx, cfg, f, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[e]], out)
                }
                val subBody = eachField.length match
                  case 0 => '{}
                  case 1 => eachField.head
                  case _ => Expr.block(eachField.init, eachField.last)
                '{
                  if $in == null then $out.burpNull()
                  else
                    $out.startElement($className)
                    $body
                    $subBody
                    $out.endElement($className)
                }
              else
                '{
                  if $in == null then $out.burpNull()
                  else
                    $out.startElement($className)
                    $body
                    $out.endElement($className)
                }
            }

          /*
          case t: JavaClassRef[?] =>
            makeWriteFnSymbol(ctx, methodKey)
            makeWriteFn[b](ctx, methodKey, aE.asInstanceOf[Expr[b]], out) { (in, out) =>
              t.refType match
                case '[p] =>
                  val rtype = t.expr.asExprOf[JavaClassRType[p]]
                  val tin = in.asExprOf[b]
                  var fieldRefs = t.fields.asInstanceOf[List[NonConstructorFieldInfoRef]]
                  val sref = ReflectOnType[String](ctx.quotes)(TypeRepr.of[String])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                  val className = Expr(t.annotations.get("co.blocke.scalajack.xmlLabel").flatMap(_.get("name")).getOrElse(t.name))
                  '{
                    if $tin == null then $out.burpNull()
                    else
                      $out.startElement($className)
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
                              MaybeWrite.maybeWriteField[e](
                                ctx,
                                cfg,
                                "", // TBD <- Should be fieldName but its at the wrong level!
                                '{ fieldValue.asInstanceOf[e] },
                                ref.fieldRef.asInstanceOf[RTypeRef[e]],
                                out
                              )
                        }
                      }
                      $out.endElement($className)
                  }
            }
           */

          case t: TraitRef[?] => throw XmlUnsupportedType("Non-sealed traits are not supported")

          case t => throw new XmlUnsupportedType("Type represented by " + t.name + " is unsupported for XML writes")

  // ---------------------------------------------------------------------------------------------

  def genWriteVal[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      aE: Expr[T],
      ref: RTypeRef[T],
      out: Expr[XmlOutput],
      inTuple: Boolean = false,
      isMapKey: Boolean = false, // only primitive or primitive-equiv types can be Map keys
      prefix: Expr[Unit],
      postfix: Expr[Unit],
      parentField: Option[FieldInfoRef],
      entryLabel: Option[String] = None,
      isStruct: Boolean = false
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
        ref match
          // First cover all primitive and simple types...
          case t: BigDecimalRef =>
            '{ $out.emitValue(${ aE.asExprOf[scala.math.BigDecimal] }.toString) }
          case t: BigIntRef =>
            '{ $out.emitValue(${ aE.asExprOf[scala.math.BigInt] }.toString) }
          case t: BooleanRef =>
            '{ $out.emitValue(${ aE.asExprOf[Boolean] }.toString) }
          case t: ByteRef =>
            '{ $out.emitValue(${ aE.asExprOf[Byte] }.toString) }
          case t: CharRef =>
            '{ $out.emitValue(${ aE.asExprOf[Char] }.toString) }
          case t: DoubleRef =>
            '{ $out.emitValue(${ aE.asExprOf[Double] }.toString) }
          case t: FloatRef =>
            '{ $out.emitValue(${ aE.asExprOf[Float] }.toString) }
          case t: IntRef =>
            '{
              $prefix
              $out.emitValue(${ aE.asExprOf[Int] }.toString)
              $postfix
            }
          case t: LongRef =>
            '{ $out.emitValue(${ aE.asExprOf[Long] }.toString) }
          case t: ShortRef =>
            '{ $out.emitValue(${ aE.asExprOf[Short] }.toString) }

          case t: StringRef =>
            // TODO: Make this work. Right now it emits the same thing, effectively ignoring the setting
            val f = parentField.getOrElse(throw new ParseError("Required parentField is empty for StringRef"))
            val labelE = Expr(f.name)
            '{
              if $aE == "" then $out.emptyElement($labelE)
              else $out.emitValue(${ aE.asExprOf[String] })
            }

          case t: JBigDecimalRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.math.BigDecimal] }.toString) }
          case t: JBigIntegerRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.math.BigInteger] }.toString) }
          case t: JBooleanRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Boolean] }.toString) }
          case t: JByteRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Byte] }.toString) }
          case t: JCharacterRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Character] }.toString) }
          case t: JDoubleRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Double] }.toString) }
          case t: JFloatRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Float] }.toString) }
          case t: JIntegerRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Integer] }.toString) }
          case t: JLongRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Long] }.toString) }
          case t: JShortRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Short] }.toString) }
          case t: JNumberRef =>
            '{ $out.emitValue(${ aE.asExprOf[java.lang.Number] }.toString) }

          case t: DurationRef       => '{ $out.emitValue(${ aE.asExprOf[java.time.Duration] }.toString) }
          case t: InstantRef        => '{ $out.emitValue(${ aE.asExprOf[java.time.Instant] }.toString) }
          case t: LocalDateRef      => '{ $out.emitValue(${ aE.asExprOf[java.time.LocalDate] }.toString) }
          case t: LocalDateTimeRef  => '{ $out.emitValue(${ aE.asExprOf[java.time.LocalDateTime] }.toString) }
          case t: LocalTimeRef      => '{ $out.emitValue(${ aE.asExprOf[java.time.LocalTime] }.toString) }
          case t: MonthDayRef       => '{ $out.emitValue(${ aE.asExprOf[java.time.MonthDay] }.toString) }
          case t: OffsetDateTimeRef => '{ $out.emitValue(${ aE.asExprOf[java.time.OffsetDateTime] }.toString) }
          case t: OffsetTimeRef     => '{ $out.emitValue(${ aE.asExprOf[java.time.OffsetTime] }.toString) }
          case t: PeriodRef         => '{ $out.emitValue(${ aE.asExprOf[java.time.Period] }.toString) }
          case t: YearRef           => '{ $out.emitValue(${ aE.asExprOf[java.time.Year] }.toString) }
          case t: YearMonthRef      => '{ $out.emitValue(${ aE.asExprOf[java.time.YearMonth] }.toString) }
          case t: ZonedDateTimeRef  => '{ $out.emitValue(${ aE.asExprOf[java.time.ZonedDateTime] }.toString) }
          case t: ZoneIdRef         => '{ $out.emitValue(${ aE.asExprOf[java.time.ZoneId] }.toString) }
          case t: ZoneOffsetRef     => '{ $out.emitValue(${ aE.asExprOf[java.time.ZoneOffset] }.toString) }

          case t: URLRef       => '{ $out.emitValue(${ aE.asExprOf[java.net.URL] }.toString) }
          case t: URIRef       => '{ $out.emitValue(${ aE.asExprOf[java.net.URI] }.toString) }
          case t: UUIDRef      => '{ $out.emitValue(${ aE.asExprOf[java.util.UUID] }.toString) }
          case t: ObjectRef[?] => '{ $out.emitValue(${ Expr(t.name) }) }

          /*
          case t: AliasRef[?] =>
            t.unwrappedType.refType match
              case '[e] =>
                genWriteVal[e](ctx, cfg, aE.asInstanceOf[Expr[e]], t.unwrappedType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple)

          case t: ArrayRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val tin = aE.asInstanceOf[Expr[Array[e]]]
                val label = collectionLabel.getOrElse(throw new XmlTypeError("Array collection label not supplied"))
                val labelE = Expr(label)
                '{
                  if $tin == null then $out.burpNull()
                  else
                    $out.startElement($labelE)
                    $tin.foreach { i =>
                      ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }
                    $out.endElement($labelE)
                }

          case t: IterableRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val tin = aE.asInstanceOf[Expr[Iterable[e]]]
                val label = collectionLabel.getOrElse(throw new XmlTypeError("Iteration collection label not supplied"))
                val labelE = Expr(label)
                '{
                  if $tin == null then $out.burpNull()
                  else
                    $out.startElement($labelE)
                    $tin.foreach { i =>
                      ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }
                    $out.endElement($labelE)
                }
           */

          case t: SeqRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Seq[e]] else aE.asExprOf[Seq[e]]
                val entryLabelE = entryLabel.map(e => Expr(e))
                val f = parentField.getOrElse(throw new ParseError("Required parentField is empty for StringRef"))
                val labelE = Expr(f.name)

                // If entryLabel not defined then we use "naked" lists: no wrapping...just repeating elements named w/fieldName
                val pprefix =
                  if entryLabel.isDefined then
                    '{
                      $out.startElement(${ entryLabelE.get })
                    }
                  else '{ () }
                val ppostfix =
                  if entryLabel.isDefined then
                    '{
                      $out.endElement(${ entryLabelE.get })
                    }
                  else '{ () }
                if isStruct then
                  '{
                    if $tin == null then $out.burpNull()
                    else if $tin.isEmpty then $out.emptyElement($labelE)
                    else
                      $tin.foreach { i =>
                        ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple, false, '{ () }, '{ () }, parentField, isStruct = isStruct) }
                      }
                  }
                else
                  '{
                    if $tin == null then $out.burpNull()
                    else if $tin.isEmpty then $out.emptyElement($labelE)
                    else
                      $prefix
                      $tin.foreach { i =>
                        ${ genWriteVal(ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple, false, pprefix, ppostfix, parentField) }
                      }
                      $postfix
                  }

          /*
          case t: SetRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Set[e]] else aE.asExprOf[Set[e]]
                val label = collectionLabel.getOrElse(throw new XmlTypeError("Set collection label not supplied"))
                val labelE = Expr(label)
                '{
                  if $tin == null then $out.burpNull()
                  else
                    $out.startElement($labelE)
                    $tin.foreach { i =>
                      ${ genWriteVal(ctx, cfg, '{ i }.asExprOf[e], t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }
                    $out.endElement($labelE)
                }

          // These are here because Enums and their various flavors can be Map keys
          // (EnumRef handles: Scala 3 enum, Scala 2 Enumeration, Java Enumeration)
          case t: EnumRef[?] =>
            val enumAsId = cfg.enumsAsIds match
              case List("-")                     => false
              case Nil                           => true
              case list if list.contains(t.name) => true
              case _                             => false
            val rtype = t.expr
            if enumAsId then
              if isMapKey then '{ $out.emitValue($rtype.asInstanceOf[EnumRType[?]].ordinal($aE.toString).get.toString) } // stringified id
              else '{ $out.emitValue($rtype.asInstanceOf[EnumRType[?]].ordinal($aE.toString).get.toString) } // int value of id
            else '{ if $aE == null then $out.burpNull() else $out.emitValue($aE.toString) }
           */

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
                      ${ genWriteVal[e](ctx, cfg, '{ vv }, t.optionParamType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple, false, prefix, postfix, parentField, entryLabel, isStruct) }
                }

          /*
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
                                case Left(v) => $out.emitValue("Left Error: " + v.toString)
                                case Right(v) =>
                                  ${ genWriteVal[rt](ctx, cfg, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out, inTuple = inTuple) }
                          }
                        case EitherLeftPolicy.THROW_EXCEPTION =>
                          '{
                            if $tin == null then $out.burpNull()
                            else
                              $tin match
                                case Left(v) => throw new XmlEitherLeftError("Left Error: " + v.toString)
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
                            case TryPolicy.ERR_MSG_STRING  => '{ $out.emitValue("Try Failure with msg: " + v.getMessage) }
                            case TryPolicy.THROW_EXCEPTION => '{ throw v }
                        }
                }
           */

          case t: MapRef[?] =>
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    val tin = if t.isMutable then aE.asExprOf[scala.collection.mutable.Map[k, v]] else aE.asExprOf[Map[k, v]]
                    val f = parentField.getOrElse(throw new ParseError("Required parentField is empty for StringRef"))
                    val labelE = Expr(f.name)
                    val _entryLabel = entryLabel.getOrElse(throw new XmlTypeError("Map entry label not supplied"))
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startElement($labelE)
                        $tin.foreach { case (key, value) =>
                          ${
                            (t.elementRef, t.elementRef2) match
                              case (aliasK: AliasRef[?], aliasV: AliasRef[?]) =>
                                aliasK.unwrappedType.refType match
                                  case '[ak] =>
                                    aliasV.unwrappedType.refType match
                                      case '[av] =>
                                        testValidMapKey(aliasK.unwrappedType)
                                        MaybeWrite.maybeWriteMapEntry[ak, av](
                                          ctx,
                                          cfg,
                                          f,
                                          _entryLabel,
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
                                    MaybeWrite.maybeWriteMapEntry[k, av](
                                      ctx,
                                      cfg,
                                      f,
                                      _entryLabel,
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
                                    MaybeWrite.maybeWriteMapEntry[ak, v](
                                      ctx,
                                      cfg,
                                      f,
                                      _entryLabel,
                                      '{ key.asInstanceOf[ak] },
                                      '{ value }.asExprOf[v],
                                      aliasK.unwrappedType.asInstanceOf[RTypeRef[ak]],
                                      t.elementRef2.asInstanceOf[RTypeRef[v]],
                                      out
                                    )
                              case (_, _) =>
                                testValidMapKey(t.elementRef)
                                MaybeWrite.maybeWriteMapEntry[k, v](
                                  ctx,
                                  cfg,
                                  f,
                                  _entryLabel,
                                  '{ key },
                                  '{ value }.asExprOf[v],
                                  t.elementRef.asInstanceOf[RTypeRef[k]],
                                  t.elementRef2.asInstanceOf[RTypeRef[v]],
                                  out
                                )
                          }
                        }
                        $out.endElement($labelE)
                    }

          /*
          case t: JavaCollectionRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val label = collectionLabel.getOrElse(throw new XmlTypeError("Map collection label not supplied"))
                val labelE = Expr(label)
                val tin = '{ $aE.asInstanceOf[java.util.Collection[?]] }
                '{
                  if $tin == null then $out.burpNull()
                  else
                    $out.startElement($labelE)
                    $tin.toArray.foreach { elem =>
                      ${ genWriteVal(ctx, cfg, '{ elem.asInstanceOf[e] }, t.elementRef.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple) }
                    }
                    $out.endElement($labelE)
                }

          case t: JavaMapRef[?] =>
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    val label = collectionLabel.getOrElse(throw new XmlTypeError("Map collection label not supplied"))
                    val labelE = Expr(label)
                    val entryLabelE = Expr("entry")
                    val tin = aE.asExprOf[java.util.Map[k, v]]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.startElement($labelE)
                        $tin.asScala.foreach { case (key, value) =>
                          ${
                            MaybeWrite.maybeWriteMapEntry[k, v](
                              ctx,
                              cfg,
                              entryLabelE,
                              '{ key },
                              '{ value }.asExprOf[v],
                              t.elementRef.asInstanceOf[RTypeRef[k]],
                              t.elementRef2.asInstanceOf[RTypeRef[v]],
                              out
                            )
                          }
                        }
                        $out.endElement($labelE)
                    }

          case t: TupleRef[?] =>
            val in = aE.asExprOf[T]
            val label = collectionLabel.getOrElse(throw new XmlTypeError("Map collection label not supplied"))
            val labelE = Expr(label)
            '{
              if $in == null then $out.burpNull()
              else
                $out.startElement($labelE)
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
                $out.endElement($labelE)
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
           */

// TODO
//          case t: AnyRef =>
//            '{ AnyWriter.writeAny(${ Expr(cfg) }, $aE, $out) }

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
            Expr.summon[XmlCodec[T]] match {
              case Some(userOverride) => '{ ${ userOverride }.encodeValue($aE, $out) }
              case None =>
                '{
                  $prefix
                  ${ genEncFnBody(ctx, cfg, ref, aE, out, '{ false }, inTuple = inTuple, isMapKey = isMapKey, parentField = parentField, isStruct = isStruct) }
                  $postfix
                }
            }
      )
