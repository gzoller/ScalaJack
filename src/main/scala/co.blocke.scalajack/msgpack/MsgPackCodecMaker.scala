package co.blocke.scalajack
package msgpack

import scala.collection.mutable.ArrayBuffer
import org.msgpack.core.{MessagePacker, MessageUnpacker, MessageFormat}
import org.msgpack.value.ValueType
import org.msgpack.core.MessagePack.PackerConfig
import org.msgpack.core.buffer.ArrayBufferOutput
import co.blocke.scala_reflection.RTypeRef
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.reflect.ClassTag
import scala.util.Success
import scala.quoted.*

object MsgPackCodecMaker:

  def generateCodecFor[T](ref: RTypeRef[T], cfg: SJConfig)(using q: Quotes)(using tt: Type[T]) =
    import q.reflect.*

    // Cache generated method Symbols + an array of the generated functions (DefDef)
    case class MethodKey(ref: RTypeRef[?], isNonConstructorFields: Boolean)
    val writeMethodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val writeMethodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]
    val readMethodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
    val readMethodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]

    inline def changeFieldName(fr: FieldInfoRef): String = fr.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(fr.name)

    def makeWriteFn[U: Type](methodKey: MethodKey, arg: Expr[U], out: Expr[MessagePacker])(f: (Expr[U], Expr[MessagePacker]) => Expr[Unit]): Expr[Unit] =
      // Get a symbol, if one already created for this key... else make one.
      Apply(
        Ref(
          writeMethodSyms.getOrElse(
            methodKey, {
              val sym = Symbol.newMethod(
                Symbol.spliceOwner,
                "w" + writeMethodSyms.size, // 'w' is for Writer!
                MethodType(List("in", "out"))(_ => List(TypeRepr.of[U], TypeRepr.of[MessagePacker]), _ => TypeRepr.of[Unit])
              )
              writeMethodSyms.update(methodKey, sym)
              writeMethodDefs += DefDef(
                sym,
                params => {
                  val List(List(in, out)) = params
                  Some(f(in.asExprOf[U], out.asExprOf[MessagePacker]).asTerm.changeOwner(sym))
                }
              )
              sym
            }
          )
        ),
        List(arg.asTerm, out.asTerm)
      ).asExprOf[Unit]

    def makeReadFn[U: Type](methodKey: MethodKey, in: Expr[MessageUnpacker])(f: Expr[MessageUnpacker] => Expr[U])(using Quotes)(using Type[MessageUnpacker]): Expr[Unit] =
      readMethodSyms.getOrElse(
        methodKey, {
          val sym = Symbol.newMethod(
            Symbol.spliceOwner,
            "r" + readMethodSyms.size,
            MethodType(List("in"))(_ => List(TypeRepr.of[MessageUnpacker]), _ => TypeRepr.of[U])
            //                    (_ => List(input_params,...), _ => resultType)
          )
          readMethodSyms.update(methodKey, sym)
          readMethodDefs += DefDef(
            sym,
            params => {
              val List(List(in)) = params
              Some(f(in.asExprOf[MessageUnpacker]).asTerm.changeOwner(sym))
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
            TypeRepr.of[Map[String,Int]],
            Flags.EmptyFlags,
            Symbol.noSymbol
          )
          classFieldMatrixSyms.update(methodKey, sym)
          val names = Expr(fieldNames)
          classFieldMatrixValDefs += ValDef(sym, Some('{ $names.zipWithIndex.toMap }.asTerm))
        }
      )
      '{}

    def maybeWrite[T](label: String, aE: Expr[T], ref: RTypeRef[T], out: Expr[MessagePacker], cfg: SJConfig): Expr[Unit] =
      val labelE = Expr(label)
      _maybeWrite[T](
        '{ $out.packString($labelE) },
        aE,
        ref,
        out,
        cfg
      )

    def _maybeWrite[T](prefix: Expr[Unit], aE: Expr[T], ref: RTypeRef[T], out: Expr[MessagePacker], cfg: SJConfig): Expr[Unit] =
      ref match
        case _ =>
          ref.refType match
            case '[u] =>
              '{
                $prefix
                ${ genWriteVal[u](aE.asExprOf[u], ref.asInstanceOf[RTypeRef[u]], out) }
              }

    def lrHasOptionChild(lr: LeftRightRef[?]): (String, Language) =
      lr.rightRef match
        case t: ScalaOptionRef[?]  => ("r", Language.Scala)
        case t: JavaOptionalRef[?] => ("r", Language.Java)
        case t: LeftRightRef[?] =>
          val (recipe, lang) = lrHasOptionChild(t)
          ("r" + recipe, lang)
        case _ =>
          lr.leftRef match
            case t: ScalaOptionRef[?]  => ("l", Language.Scala)
            case t: JavaOptionalRef[?] => ("l", Language.Java)
            case t: LeftRightRef[?] =>
              val (recipe, lang) = lrHasOptionChild(t)
              ("l" + recipe, lang)
            case _ => ("", Language.Scala)

    def tryHasOptionChild(t: TryRef[?]): (Boolean, Language) =
      t.tryRef match
        case o: ScalaOptionRef[?]  => (true, Language.Scala)
        case o: JavaOptionalRef[?] => (true, Language.Java)
        case lr: LeftRightRef[?] =>
          val (recipe, lang) = lrHasOptionChild(lr)
          (recipe.length > 0, lang)
        case _ => (false, Language.Scala)

    def genEncFnBody[T](r: RTypeRef[?], aE: Expr[T], out: Expr[MessagePacker], emitDiscriminator: Boolean = false, inTuple: Boolean = false)(using Quotes): Expr[Unit] =
      r.refType match
        case '[b] =>
          r match
            case t: ArrayRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = in.asInstanceOf[Expr[Array[e]]]
                    '{
                      if $tin == null then $out.packNil()
                      else
                        $out.packArrayHeader($tin.length)
                        $tin.foreach { i =>
                          ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                    }
              }
            case t: SeqRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                t.elementRef.refType match
                  case '[e] =>
                    val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Seq[e]] else in.asExprOf[Seq[e]]
                    '{
                      if $tin == null then $out.packNil()
                      else
                        $out.packArrayHeader($tin.length)
                        $tin.foreach { i =>
                          ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                        }
                    }
              }
            case t: ScalaClassRef[?] =>
              makeWriteFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                val body = {
                  val eachField = t.fields.map { f =>
                    f.fieldRef.refType match
                      case '[z] =>
                        val fieldValue = Select.unique(in.asTerm, f.name).asExprOf[z]
                        val fieldName = changeFieldName(f)
                        maybeWrite[z](fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out, cfg)
                  }
                  if emitDiscriminator then
                    val cname = cfg.typeHintPolicy match
                      case TypeHintPolicy.SIMPLE_CLASSNAME   => Expr(lastPart(t.name))
                      case TypeHintPolicy.SCRAMBLE_CLASSNAME => '{ scramble(${ Expr(lastPart(t.name).hashCode) }) }
                      case TypeHintPolicy.USE_ANNOTATION =>
                        Expr(t.annotations.get("co.blocke.scalajack.TypeHint").flatMap(_.get("hintValue")).getOrElse(lastPart(t.name)))
                    val withDisc = '{
                      $out.packString(${ Expr(cfg.typeHintLabel) })
                      $out.packString($cname)
                    } +: eachField
                    Expr.block(withDisc.init, withDisc.last)
                  else if eachField.length == 1 then eachField.head
                  else Expr.block(eachField.init, eachField.last)
                }

                val fieldCount = Expr(if emitDiscriminator then t.fields.size + 1 else t.fields.size)
                if !t.isCaseClass && cfg._writeNonConstructorFields then
                  val eachField = t.nonConstructorFields.map { f =>
                    f.fieldRef.refType match
                      case '[e] =>
                        val fieldValue = Select.unique(in.asTerm, f.getterLabel).asExprOf[e]
                        val fieldName = changeFieldName(f)
                        maybeWrite[e](fieldName, fieldValue, f.fieldRef.asInstanceOf[RTypeRef[e]], out, cfg)
                  }
                  val subBody = eachField.length match
                    case 0 => '{}
                    case 1 => eachField.head
                    case _ => Expr.block(eachField.init, eachField.last)
                  '{
                    if $in == null then $out.packNil()
                    else
                      $out.packMapHeader( $fieldCount )
                      $body
                      $subBody
                  }
                else
                  '{
                    if $in == null then $out.packNil()
                    else
                      $out.packMapHeader( $fieldCount )
                      $body
                  }
              }

    def genDecFnBody[T: Type](r: RTypeRef[?], in: Expr[MessageUnpacker])(using Quotes): Expr[Unit] =

      def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
        case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
        case _                        => Nil

      r.refType match // refType is Type[r.R]
        case '[b] =>
          r match
            case t: ScalaClassRef[?] =>
              makeReadFn[T](MethodKey(t, false), in)(in =>
                // Generate vars for each contractor argument, populated with either a "unit" value (eg 0, "") or given default value
                val tpe = TypeRepr.of[b]
                val classCompanion = tpe.typeSymbol.companionClass
                val companionModule = tpe.typeSymbol.companionModule
                val totalRequired = math.pow(2, t.fields.length).toInt - 1
                var required = 0
                val reqSym = Symbol.newVal(Symbol.spliceOwner, "required", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
                val allFieldNames = Expr(t.fields.map(f => changeFieldName(f)).toArray) // Used for missing required field error

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
                          else throw new MsgPackParseError("Duplicate field " + $fieldName)
                        }.asTerm
                      )
                      if dvMembers.isEmpty then
                        // no default... required?  Not if Option/Optional, or a collection
                        val unitVal = oneField.fieldRef match {
                          case _: OptionRef[?] =>
                            oneField.fieldRef.unitVal.asTerm // not required
                          case _: AnyRef =>
                            oneField.fieldRef.unitVal.asTerm // not required
                          case r: LeftRightRef[?] if r.lrkind == LRKind.EITHER => // maybe required
                            val (optionRecipe, lang) = lrHasOptionChild(r)
                            if optionRecipe.length == 0 then
                              required = required | math.pow(2, oneField.index).toInt // required
                              oneField.fieldRef.unitVal.asTerm
                            else
                              val recipeE = Expr(optionRecipe)
                              if lang == Language.Scala then
                                '{
                                  $recipeE.foldRight(None: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f]
                                }.asTerm
                              else
                                '{
                                  $recipeE.foldRight(java.util.Optional.empty: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f]
                                }.asTerm
                          case r: LeftRightRef[?] => // maybe required
                            val (optionRecipe, lang) = lrHasOptionChild(r)
                            if optionRecipe.length == 0 then // no Option children -> required
                              required = required | math.pow(2, oneField.index).toInt // required
                              oneField.fieldRef.unitVal.asTerm
                            else // at least one Option child -> optional
                            if lang == Language.Scala then '{ None }.asTerm
                            else '{ java.util.Optional.empty.asInstanceOf[f] }.asTerm
                          case y: TryRef[?] =>
                            tryHasOptionChild(y) match
                              case (true, Language.Scala) => '{ Success(None) }.asTerm
                              case (true, Language.Java)  => '{ Success(java.util.Optional.empty).asInstanceOf[f] }.asTerm
                              case _ =>
                                required = required | math.pow(2, oneField.index).toInt // required
                                oneField.fieldRef.unitVal.asTerm
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
                val caseDefsWithFinal = caseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm) // skip values of unrecognized fields

                val argss = List(idents)
                val primaryConstructor = tpe.classSymbol.get.primaryConstructor
                val constructorNoTypes = Select(New(Inferred(tpe)), primaryConstructor)
                val constructor = typeArgs(tpe) match
                  case Nil      => constructorNoTypes
                  case typeArgs => TypeApply(constructorNoTypes, typeArgs.map(Inferred(_)))
                val instantiateClass = argss.tail.foldLeft(Apply(constructor, argss.head))((acc, args) => Apply(acc, args))

                val exprRequired = Expr(required)

                makeClassFieldMatrixValDef(MethodKey(t, false), t.name.replaceAll("\\.", "_"), t.fields.map(f => changeFieldName(f)).toArray)
                val fieldMatrixSym = classFieldMatrixSyms(MethodKey(t, false)).asInstanceOf[Symbol]

                var finalVarDefs = varDefs
                val parseLoop =
                  if !cfg._writeNonConstructorFields || t.nonConstructorFields.isEmpty then
                    // When we don't care about non-constructor fields
                    '{
                      $in.getNextFormat().getValueType() match
                        case ValueType.NIL => null
                        case ValueType.MAP => 
                          (0 to $in.unpackMapHeader()-1).foreach{ _ =>
                            ${ Ref(classFieldMatrixSyms(MethodKey(t, false))).asExprOf[Map[String,Int]] }
                              .get($in.unpackString())
                              .map{ fieldNum => 
                                ${ Match('{ fieldNum }.asTerm, caseDefsWithFinal).asExprOf[Any] }
                              }
                            ()
                          }
                          if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then ${ instantiateClass.asExprOf[T] }
                          else throw new MsgPackParseError("Missing required field(s) " + ${ allFieldNames }(Integer.numberOfTrailingZeros(${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired })))
                        case fmt =>  throw new MsgPackUnexpectededType("Expected Integer value but got "+fmt) 
                    }.asTerm
                  else
                    /*
                    val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[T], Flags.Mutable, Symbol.noSymbol)
                    finalVarDefs = finalVarDefs :+ ValDef(instanceSym, Some('{ null }.asTerm)) // add var _instance=null to gen'ed code
                    val instanceSymRef = Ident(instanceSym.termRef)
                    // When we do care about non-constructor fields
                    makeClassFieldMatrixValDef(MethodKey(t, true), t.name.replaceAll("\\.", "_"), t.nonConstructorFields.sortBy(_.index).map(f => changeFieldName(f)).toArray)
                    val fieldMatrixSymNCF = classFieldMatrixSyms(MethodKey(t, true)).asInstanceOf[Symbol]
                    // New Case/Match for non-constructor fields
                    val caseDefsWithFinalNC = t.nonConstructorFields.map(ncf =>
                      ncf.fieldRef.refType match
                        case '[u] =>
                          CaseDef(
                            Literal(IntConstant(ncf.index)),
                            None,
                            // Call the setter for this field here...
                            Apply(Select.unique(Ref(instanceSym), ncf.setterLabel), List(genReadVal[u](ncf.fieldRef.asInstanceOf[RTypeRef[u]], in).asTerm)).asExpr.asTerm
                          )
                    ) :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm) // skip values of unrecognized fields
                    '{
                      val mark = $in.pos
                      var maybeFieldNum = $in.expectFirstObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })
                      if maybeFieldNum == null then null.asInstanceOf[T]
                      else
                        while maybeFieldNum.isDefined do
                          ${ Match('{ maybeFieldNum.get }.asTerm, caseDefsWithFinal).asExprOf[Any] }
                          maybeFieldNum = $in.expectObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })

                        if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then
                          ${ Assign(instanceSymRef, instantiateClass.asExpr.asTerm).asExprOf[Unit] } // _instance = (new instance)
                          $in.revertToPos(mark) // go back to re-parse object json, this time for non-constructor fields
                          maybeFieldNum = $in.expectFirstObjectField(${ Ref(fieldMatrixSymNCF).asExprOf[StringMatrix] })
                          while maybeFieldNum.isDefined do
                            ${ Match('{ maybeFieldNum.get }.asTerm, caseDefsWithFinalNC).asExprOf[Any] }
                            maybeFieldNum = $in.expectObjectField(${ Ref(fieldMatrixSymNCF).asExprOf[StringMatrix] })
                          ${ Ref(instanceSym).asExprOf[T] }
                        else throw new JsonParseError("Missing required field(s) " + ${ allFieldNames }(Integer.numberOfTrailingZeros(${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired })), $in)
                    }.asTerm
                    */
                    '{
                      null
                    }.asTerm

                Block(finalVarDefs :+ reqVarDef, parseLoop).asExprOf[T]
              )

    def genWriteVal[T: Type](
        aE: Expr[T],
        ref: RTypeRef[T],
        out: Expr[MessagePacker],
        inTuple: Boolean = false
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
              throw MsgPackUnsupportedType("BigDecimal not supported for MsgPack format")
            case t: BigIntRef =>
              '{ if $aE == null then $out.packNil() else $out.packBigInteger($aE.bigInteger) }
            case t: BooleanRef =>
              '{ $out.packBoolean($aE) }
            case t: ByteRef =>
              '{ $out.packByte($aE) }
            case t: CharRef =>
              '{ $out.packByte($aE.toByte) }
            case t: DoubleRef =>
              '{ $out.packDouble($aE) }
            case t: FloatRef =>
              '{ $out.packFloat($aE) }
            case t: IntRef =>
              '{ $out.packInt($aE) }
            case t: LongRef =>
              '{ $out.packLong($aE) }
            case t: ShortRef =>
              '{ $out.packShort($aE) }

            case t: StringRef =>
              '{ if $aE == null then $out.packNil() else $out.packString($aE) }

            case t: JBigDecimalRef =>
              throw MsgPackUnsupportedType("BigDecimal not supported for MsgPack format")
            case t: JBigIntegerRef =>
              '{ if $aE == null then $out.packNil() else $out.packBigInteger($aE) }
            case t: JBooleanRef =>
              '{ $out.packBoolean(${aE.asExprOf[Boolean]}) }
            case t: JByteRef =>
              '{ $out.packByte(${aE.asExprOf[Byte]}) }
            case t: JCharacterRef =>
              '{ $out.packByte($aE.toByte) }
            case t: JDoubleRef =>
              '{ $out.packDouble(${ aE.asExprOf[Double] }) }
            case t: JFloatRef =>
              '{ $out.packFloat(${ aE.asExprOf[Float] }) }
            case t: JIntegerRef =>
              '{ $out.packInt(${ aE.asExprOf[Int] }) }
            case t: JLongRef =>
              '{ $out.packLong(${ aE.asExprOf[Long] }) }
            case t: JShortRef =>
              '{ $out.packShort(${ aE.asExprOf[Short] }) }
            case t: JNumberRef =>
              throw MsgPackUnsupportedType("Number not supported for MsgPack format")

              /*
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
              */

              /*
            case t: AliasRef[?] =>
              // Special check for RawJson pseudo-type
              if lastPart(t.definedType) == "RawJson" then '{ $out.valueRaw(${ aE.asExprOf[RawJson] }) }
              else
                t.unwrappedType.refType match
                  case '[e] =>
                    genWriteVal[e](aE.asInstanceOf[Expr[e]], t.unwrappedType.asInstanceOf[RTypeRef[e]], out, inTuple = inTuple)
                    */

            // These are here becaue Enums and their various flavors can be Map keys
            // (EnumRef handles: Scala 3 enum, Scala 2 Enumeration, Java Enumeration)
            /*
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
              */

            // NeoType is a bit of a puzzle-box.  To get the correct underlying base type, I had to dig into
            // the argument of method validate$retainedBody.  It happened to have the correctly-typed parameter.
            // With the correct type, we can correct write out the value.
            /*
            case t: NeoTypeRef[?] => // in Quotes context
              Symbol.requiredModule(t.typedName.toString).methodMember("validate$retainedBody").head.paramSymss.head.head.tree match
                case ValDef(_, tt, _) =>
                  tt.tpe.asType match
                    case '[u] =>
                      val baseTypeRef = ReflectOnType.apply(q)(tt.tpe)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
                      genWriteVal[u]('{ $aE.asInstanceOf[u] }, baseTypeRef.asInstanceOf[RTypeRef[u]], out)
                    */

            // case t: AnyRef => '{ AnyWriter.writeAny($aE, $out, ${ Expr(cfg) }) }

            // Everything else...
            case _ => genEncFnBody(ref, aE, out, inTuple = inTuple)
        )

    def genReadVal[T: Type](
        ref: RTypeRef[T],
        in: Expr[MessageUnpacker],
        inTuple: Boolean = false // not sure if needed...
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
              throw MsgPackUnsupportedType("BigDecimal not supported for MsgPack format")
            case t: BigIntRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => scala.math.BigInt($in.unpackBigInteger())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer value but got "+fmt) 
              }.asExprOf[T]
            case t: BooleanRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.BOOLEAN => $in.unpackBoolean()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Boolean value but got "+fmt) 
              }.asExprOf[T]
            case t: ByteRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.INTEGER => $in.unpackByte()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (byte) value but got "+fmt) 
              }.asExprOf[T]
            case t: CharRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.INTEGER => $in.unpackByte().toChar
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (byte/char) value but got "+fmt) 
              }.asExprOf[T]
            case t: DoubleRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.FLOAT => $in.unpackDouble()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Float (double) value but got "+fmt) 
              }.asExprOf[T]
            case t: FloatRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.FLOAT => $in.unpackFloat()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Float value but got "+fmt) 
              }.asExprOf[T]
            case t: IntRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.INTEGER => $in.unpackInt()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer value but got "+fmt) 
              }.asExprOf[T]
            case t: LongRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.INTEGER => $in.unpackLong()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (long) value but got "+fmt) 
              }.asExprOf[T]
            case t: ShortRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.INTEGER => $in.unpackShort()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (short) value but got "+fmt) 
              }.asExprOf[T]
            case t: StringRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.STRING => $in.unpackString()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected String value but got "+fmt) 
              }.asExprOf[T]

            case t: JBigDecimalRef =>
              throw MsgPackUnsupportedType("BigDecimal not supported for MsgPack format")
            case t: JBigIntegerRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => $in.unpackBigInteger()
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer value but got "+fmt) 
              }.asExprOf[T]
            case t: JBooleanRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.BOOLEAN => java.lang.Boolean.valueOf($in.unpackBoolean())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Boolean value but got "+fmt) 
              }.asExprOf[T]
            case t: JByteRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => java.lang.Byte.valueOf($in.unpackByte())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (byte) value but got "+fmt) 
              }.asExprOf[T]
            case t: JCharacterRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => java.lang.Character.valueOf($in.unpackByte().toChar)
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (byte/char) value but got "+fmt) 
              }.asExprOf[T]
            case t: JDoubleRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.FLOAT => java.lang.Double.valueOf($in.unpackDouble())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Float (double) value but got "+fmt) 
              }.asExprOf[T]
            case t: JFloatRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.FLOAT => java.lang.Float.valueOf($in.unpackFloat())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Float value but got "+fmt) 
              }.asExprOf[T]
            case t: JIntegerRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER =>java.lang.Integer.valueOf( $in.unpackInt())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer value but got "+fmt) 
              }.asExprOf[T]
            case t: JLongRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => java.lang.Long.valueOf($in.unpackLong())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (long) value but got "+fmt) 
              }.asExprOf[T]
            case t: JShortRef =>
              '{
                $in.getNextFormat().getValueType() match
                  case ValueType.NIL => null
                  case ValueType.INTEGER => java.lang.Short.valueOf($in.unpackShort())
                  case fmt =>  throw new MsgPackUnexpectededType("Expected Integer (short) value but got "+fmt) 
              }.asExprOf[T]
            case t: JNumberRef =>
              throw MsgPackUnsupportedType("Number not supported for MsgPack format")

            case t: SeqRef[?] =>
              ref.refType match
                case '[List[?]] =>
                  t.elementRef.refType match
                    case '[e] =>
                      val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                      '{
                        $in.getNextFormat().getValueType() match
                          case ValueType.NIL => null
                          case ValueType.ARRAY => 
                            (0 to $in.unpackArrayHeader()-1).map(_ => ${ genReadVal[e](rtypeRef, in, inTuple) }).toList
                          case fmt => throw new MsgPackUnexpectededType("Expected Array value but got "+fmt) 
                      }.asExprOf[T]

            case t: ArrayRef[?] =>
              t.elementRef.refType match
                case '[e] =>
                  val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                  val ct = Expr.summon[ClassTag[e]].get
                  '{
                    $in.getNextFormat().getValueType() match
                      case ValueType.NIL => null
                      case ValueType.ARRAY => 
                        val parsedArray = (0 to $in.unpackArrayHeader()-1).map(_ => ${ genReadVal[e](rtypeRef, in, inTuple) })
                        implicit val ctt = $ct
                        parsedArray.toArray[e]
                      case fmt => throw new MsgPackUnexpectededType("Expected Array value but got "+fmt) 
                  }.asExprOf[T]

            case _ =>
              // Classes, traits, etc.
              genDecFnBody[T](ref, in) // Create a new decoder function (presumably for class, trait, etc)
              genReadVal(ref, in, inTuple)
        )

    val codecDef = '{ // FIXME: generate a type class instance using `ClassDef.apply` and `Symbol.newClass` calls after graduating from experimental API: https://www.scala-lang.org/blog/2022/06/21/scala-3.1.3-released.html
      new MsgPackCodec[T] {
        def encodeValue(in: T, out: MessagePacker): Unit = ${ genWriteVal('in, ref, 'out) }
        def decodeValue(in: MessageUnpacker): T = ${ genReadVal(ref, 'in) }
      }
    }.asTerm
    val codec = Block((classFieldMatrixValDefs ++ writeMethodDefs ++ readMethodDefs).toList, codecDef).asExprOf[MsgPackCodec[T]]
    // println(s"Codec: ${codec.show}")
    codec
