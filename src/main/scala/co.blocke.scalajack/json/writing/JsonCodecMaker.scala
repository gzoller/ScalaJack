package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*

object JsonCodecMaker:

  def generateCodecFor[T](ref: RTypeRef[T], cfg: JsonConfig)(using Quotes)(using tt: Type[T]) =
    import quotes.reflect.*

    // Cache generated method Symbols + an array of the generated functions (DefDef)
    case class MethodKey(ref: RTypeRef[?], isStringified: Boolean) // <-- TODO: Not clear what isStringified does here...
    val methodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val methodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]

    // Fantastic Dark Magic here--lifted from Jasoniter.  Props!  This thing will create a DefDef, and a Symbol to it.
    // The Symbol will let you call the generated function later from other macro-generated code.  The goal is to use
    // generated functions to create cleaner/faster macro code than what straight quotes/splices would create unaided.
    def makeFn[U: Type](methodKey: MethodKey, arg: Expr[U], out: Expr[JsonOutput])(f: (Expr[U], Expr[JsonOutput]) => Expr[Unit]): Expr[Unit] =
      // Get a symbol, if one already created for this key... else make one.
      Apply(
        Ref(
          methodSyms.getOrElse(
            methodKey, {
              val sym = Symbol.newMethod(
                Symbol.spliceOwner,
                "w" + methodSyms.size, // 'w' is for Writer!
                MethodType(List("in", "out"))(_ => List(TypeRepr.of[U], TypeRepr.of[JsonOutput]), _ => TypeRepr.of[Unit])
              )
              methodSyms.update(methodKey, sym)
              methodDefs += DefDef(
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

    def genFnBody[T](r: RTypeRef[?], aE: Expr[T], out: Expr[JsonOutput], emitDiscriminator: Boolean = false)(using Quotes): Expr[Unit] =
      r.refType match
        case '[b] =>
          r match
            case t: ArrayRef[?] =>
              makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = in.asExprOf[Array[e]]
                    '{
                      $out.startArray()
                      $tin.foreach { i =>
                        ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                      }
                      $out.endArray()
                    }
              }

            case t: SeqRef[?] =>
              makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Seq[e]] else in.asExprOf[Seq[e]]
                    '{
                      $out.startArray()
                      $tin.foreach { i =>
                        ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                      }
                      $out.endArray()
                    }
              }

            case t: SetRef[?] =>
              makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Set[e]] else in.asExprOf[Set[e]]
                    '{
                      $out.startArray()
                      $tin.foreach { i =>
                        ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                      }
                      $out.endArray()
                    }
              }

            case t: ScalaClassRef[?] =>
              makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                val tin = in.asExprOf[b]
                val body = {
                  val eachField = t.fields.map { f =>
                    f.fieldRef.refType match
                      case '[z] =>
                        val fname = Expr(f.name)
                        val fieldValue = Select.unique(tin.asTerm, f.name).asExprOf[z]
                        '{
                          $out.label($fname)
                          ${ genWriteVal(fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out) }
                        }
                  }
                  if emitDiscriminator then
                    val cname = cfg.typeHintPolicy match
                      case TypeHintPolicy.SIMPLE_CLASSNAME => Expr(lastPart(t.name))
                      case TypeHintPolicy.SCRAMBLE_CLASSNAME =>
                        val hash = Expr(lastPart(t.name).hashCode)
                        '{ scramble($hash) }
                      case TypeHintPolicy.USE_ANNOTATION => ???
                    val withDisc = '{
                      $out.label(${ Expr(cfg.typeHintLabel) })
                      $out.value($cname)
                    } +: eachField
                    Expr.block(withDisc.init, withDisc.last)
                  else if eachField.length == 1 then eachField.head
                  else Expr.block(eachField.init, eachField.last)
                }
                '{
                  $out.startObject()
                  $body
                  $out.endObject()
                }
              }

            case t: MapRef[?] =>
              t.elementRef.refType match
                case '[k] =>
                  makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                    t.elementRef2.refType match
                      case '[v] =>
                        val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Map[k, v]] else in.asExprOf[Map[k, v]]
                        '{
                          $out.startObject()
                          $tin.foreach { case (k, v) =>
                            $out.maybeComma()
                            ${ genWriteVal('{ k }, t.elementRef.asInstanceOf[RTypeRef[k]], out, true) }
                            $out.colon()
                            ${ genWriteVal('{ v }, t.elementRef2.asInstanceOf[RTypeRef[v]], out) }
                          }
                          $out.endObject()
                        }
                  }

            case t: TraitRef[?] =>
              // classesSeen.put(t.typedName, t)
              // val rt = t.expr.asInstanceOf[Expr[TraitRType[T]]]
              if !t.isSealed then throw new JsonUnsupportedType("Non-sealed traits are not supported")
              if t.childrenAreObject then
                // case object -> just write the simple name of the object
                val tin = aE.asExprOf[b]
                '{
                  $out.value($tin.getClass.getName.split('.').last.stripSuffix("$"))
                }
              else
                // So... sealed trait children could be any of those defined for the trait.  We need to
                // generate functions for each child then a master function that examines $aE and based on
                // its value, call the appropriate function to render.
                // val beforeKeys = methodSyms.keySet
                // t.sealedChildren.foreach { child =>
                //   child.refType match
                //     case '[c] =>
                //       genFnBody[c](child, aE.asExprOf[c], out)
                // }
                // Now generate and return the calling function based on runtime type
                // Refer to Jsoniter: JsonCodecMaker.scala around line 920 for example how to do this, incl a wildcard, which
                // we don't need here.
                val cases = t.sealedChildren.map { child =>
                  child.refType match
                    case '[c] =>
                      val subtype = TypeIdent(TypeRepr.of[c].typeSymbol)
                      val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, subtype.tpe)
                      CaseDef(Bind(sym, Typed(Ref(sym), subtype)), None, genFnBody[c](child, Ref(sym).asExprOf[c], out, true).asTerm)
                } :+ CaseDef(Literal(NullConstant()), None, '{ $out.burpNull() }.asTerm)
                val matchExpr = Match(aE.asTerm, cases).asExprOf[Unit]
                matchExpr

    def genWriteVal[T: Type](
        aE: Expr[T],
        ref: RTypeRef[T],
        // optWriteDiscriminator: Option[WriteDiscriminator],
        out: Expr[JsonOutput],
        // cfgE: Expr[JsonConfig],
        isStringified: Boolean = false // e.g. Map key values.  Doesn't apply to stringish values, which are always quotes-wrapped
    )(using Quotes): Expr[Unit] =
      val methodKey = MethodKey(ref, false)
      methodSyms
        .get(methodKey)
        .map { sym => // hit cache first... then match on Ref type
          Apply(Ref(sym), List(aE.asTerm, out.asTerm)).asExprOf[Unit]
        }
        .getOrElse(
          ref match
            // First cover all primitive and simple types...
            case t: BigDecimalRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[scala.math.BigDecimal] }) }
              else '{ $out.value(${ aE.asExprOf[scala.math.BigDecimal] }) }
            case t: BigIntRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[scala.math.BigInt] }) }
              else '{ $out.value(${ aE.asExprOf[scala.math.BigInt] }) }
            case t: BooleanRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Boolean] }) }
              else '{ $out.value(${ aE.asExprOf[Boolean] }) }
            case t: ByteRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Byte] }) }
              else '{ $out.value(${ aE.asExprOf[Byte] }) }
            case t: CharRef => '{ $out.value(${ aE.asExprOf[Char] }) }
            case t: DoubleRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Double] }) }
              else '{ $out.value(${ aE.asExprOf[Double] }) }
            case t: FloatRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Float] }) }
              else '{ $out.value(${ aE.asExprOf[Float] }) }
            case t: IntRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Int] }) }
              else '{ $out.value(${ aE.asExprOf[Int] }) }
            case t: LongRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Long] }) }
              else '{ $out.value(${ aE.asExprOf[Long] }) }
            case t: ShortRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[Short] }) }
              else '{ $out.value(${ aE.asExprOf[Short] }) }
            case t: StringRef => '{ $out.value(${ aE.asExprOf[String] }) }

            case t: JBigDecimalRef =>
              if isStringified then '{ $out.valueStringified(${ aE.asExprOf[java.math.BigDecimal] }) }
              else '{ $out.value(${ aE.asExprOf[java.math.BigDecimal] }) }
            case t: JBigIntegerRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.math.BigInteger] }) }
              else '{ $out.value(${ aE.asExprOf[java.math.BigInteger] }) }
            case t: JBooleanRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Boolean] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Boolean] }) }
            case t: JByteRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Byte] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Byte] }) }
            case t: JCharacterRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Character] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Character] }) }
            case t: JDoubleRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Double] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Double] }) }
            case t: JFloatRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Float] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Float] }) }
            case t: JIntegerRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Integer] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Integer] }) }
            case t: JLongRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Long] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Long] }) }
            case t: JShortRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Short] }) }
              else '{ $out.value(${ aE.asExprOf[java.lang.Short] }) }
            case t: JNumberRef =>
              if isStringified then '{ $out.value(${ aE.asExprOf[java.lang.Number] }) }
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

            case t: UUIDRef => '{ $out.value(${ aE.asExprOf[java.util.UUID] }) }

            case t: AliasRef[?] =>
              t.unwrappedType.refType match
                case '[e] =>
                  genWriteVal[e](aE.asInstanceOf[Expr[e]], t.unwrappedType.asInstanceOf[RTypeRef[e]], out)

            // Everything else...
            case _ if isStringified => throw new JsonIllegalKeyType("Non-primitive/non-simple types cannot be map keys")
            case _                  => genFnBody(ref, aE, out)
        )

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
      }
    }.asTerm
    val neededDefs =
      // others here???  Refer to Jsoniter file JsonCodecMaker.scala
      methodDefs
    val codec = Block(neededDefs.toList, codecDef).asExprOf[JsonCodec[T]]
    // println(s"Codec: ${codec.show}")
    codec
