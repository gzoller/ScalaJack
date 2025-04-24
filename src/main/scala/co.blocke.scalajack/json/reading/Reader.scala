package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import scala.util.{Failure, Success, Try}
import scala.collection.Factory
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RType, RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.rtypes.EnumRType

object Reader:

  private def makeReadFn[U: Type](
      ctx: CodecBuildContext,
      methodKey: TypedName,
      in: Expr[JsonSource]
  )(f: Expr[JsonSource] => Expr[U]): Expr[U] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    println("    @ Making reader fn for " + methodKey)
    val sym =
      ctx.readMethodSyms.getOrElseUpdate(
        methodKey, {
          val newSym = Symbol.newMethod(
            Symbol.spliceOwner,
            "r" + ctx.readMethodSyms.size,
            MethodType(List("in"))(
              _ => List(TypeRepr.of[JsonSource]),
              _ => TypeRepr.of[U]
            )
          )
          ctx.readMethodDefs.update(
            methodKey,
            DefDef(
              newSym,
              params => {
                val List(List(inParam)) = params
                Some(f(inParam.asExprOf[JsonSource]).asTerm.changeOwner(newSym))
              }
            )
          )
          newSym
        }
      )
    println("    @ Done making reader fn for " + methodKey)

    // Populate readerMap for SelfRef lookups (Map[TypedName->reader_fn]). Avoids very nasty recusion/scope issues
    ctx.readerFnMapEntries(methodKey) = '{ (in: JsonSource) => ${ Apply(Ref(sym), List('in.asTerm)).asExprOf[Any] } }

    // Always apply the function — whether from cache or new
    Apply(Ref(sym), List(in.asTerm)).asExprOf[U]

  private def makeClassFieldMatrixValDef(
      ctx: CodecBuildContext,
      methodKey: TypedName,
      className: String,
      fieldNames: Array[String]
  ): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    ctx.classFieldMatrixSyms.getOrElse(
      methodKey, {
        val sym = Symbol.newVal(
          Symbol.spliceOwner,
          s"__$className" + "_fields",
          TypeRepr.of[StringMatrix],
          Flags.EmptyFlags,
          Symbol.noSymbol
        )
        ctx.classFieldMatrixSyms.update(methodKey, sym)
        val names = Expr(fieldNames)
        ctx.classFieldMatrixValDefs += ValDef(sym, Some('{ new StringMatrix($names) }.asTerm))
      }
    )
    '{}

// ---------------------------------------------------------------------------------------------

  private def lrHasOptionChild(lr: LeftRightRef[?]): (String, Language) =
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

  private def tryHasOptionChild(t: TryRef[?]): (Boolean, Language) =
    t.tryRef match
      case o: ScalaOptionRef[?]  => (true, Language.Scala)
      case o: JavaOptionalRef[?] => (true, Language.Java)
      case lr: LeftRightRef[?] =>
        val (recipe, lang) = lrHasOptionChild(lr)
        (recipe.nonEmpty, lang)
      case _ => (false, Language.Scala)

  private def testValidMapKey(testRef: RTypeRef[?]): Boolean =
    val isValid = testRef match
      case _: PrimitiveRef                       => true
      case _: TimeRef                            => true
      case _: NetRef                             => true
      case c: ScalaClassRef[?] if c.isValueClass => true
      case _: EnumRef[?]                         => true
      case a: AliasRef[?]                        => testValidMapKey(a.unwrappedType)
      case t: TraitRef[?] if t.childrenAreObject => true
      case _                                     => false
    if !isValid then throw new JsonTypeError(s"For JSON serialization, map keys must be a simple type. ${testRef.name} is too complex.")
    isValid

  // ---------------------------------------------------------------------------------------------

  private def genDecFnBody[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      r: RTypeRef[?],
      in: Expr[JsonSource]
  ): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
      case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
      case _                        => Nil

    val methodKey = r.typedName
    r.refType match // refType is Type[r.R]
      case '[b] =>
        r match

          case t: Sealable if t.isSealed && t.childrenAreObject => // case objects
            makeReadFn[b](ctx, methodKey, in)(in =>
              val classPrefixE = Expr(allButLastPart(t.name))
              val caseDefs = t.sealedChildren.map { childRef =>
                val nameE = Expr(childRef.name)
                childRef.refType match
                  case '[o] =>
                    CaseDef(
                      Literal(StringConstant(childRef.name)),
                      None,
                      Ref(TypeRepr.of[o].typeSymbol).asExprOf[o].asTerm
                    )
              }
              '{
                if $in.expectNull() then null
                else ${ Match('{ $classPrefixE + "." + $in.expectString() }.asTerm, caseDefs).asExprOf[T] }
              }.asExprOf[b]
            )
            '{ () }

          case t: Sealable if t.isSealed && !t.childrenAreObject =>
            println("!!! In Sealable")
            // This is a massive side effect: go ahead and generate readers for every sealed child. This doesn't
            // return anything but (...and here's the side effect) it updates a cache of readers
            t.sealedChildren.map(kid =>
              kid.refType match
                case '[c] =>
                  genDecFnBody[c](ctx, cfg, kid.asInstanceOf[RTypeRef[c]], in)
            )
            makeReadFn[T](ctx, methodKey, in)(in =>
              val hintLabelE = Expr(cfg.typeHintLabel)
              val classPrefixE = Expr(allButLastPart(t.name))
              val caseDefs = t.sealedChildren.map { childRef =>
                val childNameE = Expr(childRef.name)
                cfg.typeHintPolicy match
                  case TypeHintPolicy.SCRAMBLE_CLASSNAME =>
                    val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, TypeRepr.of[String])
                    CaseDef(
                      Bind(sym, Typed(Wildcard(), Inferred(TypeRepr.of[String]))),
                      Some({
                        val tE = Ref(sym).asExprOf[String]
                        '{ descramble($tE, lastPart($childNameE).hashCode) }.asTerm
                      }), {
                        val methodKey = childRef.typedName
                        ctx.readMethodSyms
                          .get(methodKey)
                          .map { sym =>
                            Apply(Ref(sym), List(in.asTerm)).asExprOf[T]
                          }
                          .get
                          .asTerm
                      }
                    )
                  case TypeHintPolicy.USE_ANNOTATION =>
                    val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, TypeRepr.of[String])
                    val annoOrName =
                      childRef match
                        case cr: ClassRef[?] => cr.annotations.get("co.blocke.scalajack.TypeHint").flatMap(_.get("hintValue")).getOrElse(lastPart(cr.name))
                        case _               => lastPart(childRef.name)
                    CaseDef(
                      Literal(StringConstant(annoOrName)),
                      None, {
                        val methodKey = childRef.typedName
                        ctx.readMethodSyms
                          .get(methodKey)
                          .map { sym =>
                            Apply(Ref(sym), List(in.asTerm)).asExprOf[T]
                          }
                          .get
                          .asTerm
                      }
                    )
                  case TypeHintPolicy.SIMPLE_CLASSNAME =>
                    CaseDef(
                      Literal(StringConstant(childRef.name)),
                      None, {
                        val methodKey = childRef.typedName
                        ctx.readMethodSyms
                          .get(methodKey)
                          .map { sym =>
                            Apply(Ref(sym), List(in.asTerm)).asExprOf[T]
                          }
                          .get
                          .asTerm
                      }
                    )
              }
              if cfg._preferTypeHints then
                '{
                  if $in.expectNull() then null
                  else
                    val hint = $in.findObjectField($hintLabelE).getOrElse(throw JsonParseError(s"Unable to find type hint for abstract class $$cnameE", $in))
                    ${
                      cfg.typeHintPolicy match
                        case TypeHintPolicy.SIMPLE_CLASSNAME   => Match('{ $classPrefixE + "." + hint }.asTerm, caseDefs).asExprOf[T]
                        case TypeHintPolicy.SCRAMBLE_CLASSNAME => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                        case TypeHintPolicy.USE_ANNOTATION     => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                    }
                }.asExprOf[T]
              else
                val unique = Unique.findUniqueWithExcluded(t)
                val excludeFields = Expr(unique.optionalFields)
                val liftedUnique = liftStringMap(unique.simpleUniqueHash)
                val tname = Expr(t.name)
                val matchCases: List[CaseDef] = t.sealedChildren.flatMap { classRef =>
                  val methodKey = classRef.typedName
                  ctx.readMethodSyms.get(methodKey).map { sym =>
                    val cond = Literal(StringConstant(classRef.name))
                    val rhs = Apply(Ref(sym), List(in.asTerm)).asExprOf[T].asTerm
                    CaseDef(cond, None, rhs)
                  }
                }

                '{
                  if $in.expectNull() then null
                  else {
                    // 1. See if type hint is present--if so, use it!
                    $in.findObjectField($hintLabelE) match {
                      case Some(hint) =>
                        ${
                          cfg.typeHintPolicy match
                            case TypeHintPolicy.SIMPLE_CLASSNAME   => Match('{ $classPrefixE + "." + hint }.asTerm, caseDefs).asExprOf[T]
                            case TypeHintPolicy.SCRAMBLE_CLASSNAME => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                            case TypeHintPolicy.USE_ANNOTATION     => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                        }
                      case None =>
                        // 2. Find all field names
                        val fields = $in.findAllFieldNames()
                        // 3. Strip out all excluded (optional) fields and make hash
                        val fingerprint = Unique.hashOf(fields.filterNot($excludeFields.contains))
                        // 4. Look up hash
                        val className = $liftedUnique
                          .get(fingerprint)
                          .getOrElse(
                            // Before admitting failure, it is possible "" is a valid hash key!
                            $liftedUnique.get("").getOrElse(throw new JsonParseError("Class in trait " + $tname + s" with parsed fields [${fields.mkString(",")}] needed a type hint but none was found (ambiguous)", $in))
                          )
                        ${ Match('{ className }.asTerm, matchCases).asExprOf[T] }
                    }
                  }
                }.asExprOf[T]
            )
            '{ () }

          case t: TraitRef[?] =>
            throw JsonUnsupportedType("Non-sealed traits are not supported")

          case t: ScalaClassRef[?] =>
            println("!!! In Class")
            makeReadFn[b](ctx, methodKey, in)(in =>
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
                          ${ Assign(fieldSymRef, genReadVal[f](ctx, cfg, oneField.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit] }
                        else throw new JsonParseError("Duplicate field " + $fieldName, $in)
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
                          if optionRecipe.isEmpty then
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
                          if optionRecipe.isEmpty then // no Option children -> required
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

              makeClassFieldMatrixValDef(ctx, methodKey, t.name.replaceAll("\\.", "_"), t.fields.map(f => changeFieldName(f)).toArray)
              val fieldMatrixSym = ctx.classFieldMatrixSyms(methodKey)

              var finalVarDefs = varDefs
              val parseLoop =
                if !cfg._writeNonConstructorFields || t.nonConstructorFields.isEmpty then
                  // When we don't care about non-constructor fields
                  '{
                    var maybeFieldNum = $in.expectFirstObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })
                    if maybeFieldNum == null then null.asInstanceOf[T]
                    else
                      while maybeFieldNum.isDefined do
                        ${ Match('{ maybeFieldNum.get }.asTerm, caseDefsWithFinal).asExprOf[Any] }
                        maybeFieldNum = $in.expectObjectField(${ Ref(fieldMatrixSym).asExprOf[StringMatrix] })

                      if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then ${ instantiateClass.asExprOf[T] }
                      else throw new JsonParseError("Missing required field(s) " + ${ allFieldNames }(Integer.numberOfTrailingZeros(${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired })), $in)
                  }.asTerm
                else
                  val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[T], Flags.Mutable, Symbol.noSymbol)
                  finalVarDefs = finalVarDefs :+ ValDef(instanceSym, Some('{ null }.asTerm)) // add var _instance=null to gen'ed code
                  val instanceSymRef = Ident(instanceSym.termRef)
                  // When we do care about non-constructor fields
                  makeClassFieldMatrixValDef(ctx, methodKey, t.name.replaceAll("\\.", "_"), t.nonConstructorFields.sortBy(_.index).map(f => changeFieldName(f)).toArray)
                  val fieldMatrixSymNCF = ctx.classFieldMatrixSyms(methodKey)
                  // New Case/Match for non-constructor fields
                  val caseDefsWithFinalNC = t.nonConstructorFields.map(ncf =>
                    ncf.fieldRef.refType match
                      case '[u] =>
                        CaseDef(
                          Literal(IntConstant(ncf.index)),
                          None,
                          // Call the setter for this field here...
                          Apply(Select.unique(Ref(instanceSym), ncf.setterLabel), List(genReadVal[u](ctx, cfg, ncf.fieldRef.asInstanceOf[RTypeRef[u]], in).asTerm)).asExpr.asTerm
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

              Block(finalVarDefs :+ reqVarDef, parseLoop).asExprOf[b]
            )
            '{ () }

          case t => throw new ParseError("Not yet implemented: " + t)

// ---------------------------------------------------------------------------------------------

  def genReadVal[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      ref: RTypeRef[T],
      in: Expr[JsonSource],
      inTuple: Boolean = false,
      isMapKey: Boolean = false
  )(using Quotes): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val methodKey = ref.typedName
    ctx.readMethodSyms
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

          case t: URLRef  => '{ $in.expectString((s: String) => new java.net.URI(s).toURL()) }.asExprOf[T]
          case t: URIRef  => '{ $in.expectString((s: String) => new java.net.URI(s)) }.asExprOf[T]
          case t: UUIDRef => '{ $in.expectString(java.util.UUID.fromString) }.asExprOf[T]

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
                    else Some(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in) })
                  }.asExprOf[T]
                else
                  '{
                    if $in.expectNull() then null
                    else ${ ofOption[e](Some(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in))).asExprOf[T] }
                  }.asExprOf[T]
          case t: JavaOptionalRef[?] =>
            import quotes.reflect.*
            t.optionParamType.refType match
              case '[e] =>
                if cfg.noneAsNull || inTuple then
                  '{
                    if $in.expectNull() then java.util.Optional.empty
                    else java.util.Optional.of(${ genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in) })
                  }.asExprOf[T]
                else
                  '{
                    if $in.expectNull() then null
                    else ${ ofOptional[e](java.util.Optional.of(genReadVal[e](ctx, cfg, t.optionParamType.asInstanceOf[RTypeRef[e]], in))).asExprOf[T] }
                  }.asExprOf[T]
          // else ofOption[e](Some(genReadVal[e](t.optionParamType.asInstanceOf[RTypeRef[e]], in))).asExprOf[T]

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
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then parsedArray.toList
                      else null
                    }.asExprOf[T]
              case '[Vector[?]] =>
                t.elementRef.refType match
                  case '[e] =>
                    val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then parsedArray.toVector
                      else null
                    }.asExprOf[T]
              case '[IndexedSeq[?]] =>
                t.elementRef.refType match
                  case '[e] =>
                    val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then parsedArray.toIndexedSeq
                      else null
                    }.asExprOf[T]
              case '[Seq[?]] =>
                t.elementRef.refType match
                  case '[e] =>
                    val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then parsedArray.toSeq
                      else null
                    }.asExprOf[T]
              // Catch all, with (slightly) slower type coersion to proper Seq flavor
              case _ =>
                t.elementRef.refType match
                  case '[e] =>
                    val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then parsedArray.to(${ Expr.summon[Factory[e, T]].get }) // create appropriate flavor of Seq[T] here
                      else null
                    }.asExprOf[T]

          case t: IterableRef[?] =>
            t.elementRef.refType match
              case '[e] =>
                val rtypeRef = t.elementRef.asInstanceOf[RTypeRef[e]]
                val ct = Expr.summon[ClassTag[e]].get
                '{
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
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
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
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
                  val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
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
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray == null then null
                      else new java.util.concurrent.ArrayBlockingQueue(parsedArray.length, true, parsedArray.toList.asJava)
                    }.asExprOf[T]
                  // java.util.Collection interfaces
                  case "java.util.Stack" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray == null then null
                      else
                        val s = new java.util.Stack[e]()
                        parsedArray.map(j => s.push(j))
                        s
                    }.asExprOf[T]
                  case "java.util.concurrent.TransferQueue" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then new java.util.concurrent.LinkedTransferQueue(parsedArray.toList.asJava)
                      else null
                    }.asExprOf[T]
                  case "java.util.concurrent.BlockingQueue" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then new java.util.concurrent.LinkedBlockingQueue(parsedArray.toList.asJava)
                      else null
                    }.asExprOf[T]
                  case "java.util.TreeSet" | "java.util.NavigableSet" | "java.util.SortedSet" =>
                    t.elementRef match
                      case _: PrimitiveRef =>
                        t.elementRef.refType match
                          case '[z] =>
                            '{
                              val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                              if parsedArray != null then new java.util.TreeSet(parsedArray.toList.asJava)
                              else null
                            }.asExprOf[T]
                      case x => throw JsonTypeError("Only primitive types supported for TreeSet, NavigableSet, or SortedSet. You've specified " + x.name)
                  case "java.util.Queue" | "java.util.Deque" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then new java.util.LinkedList(parsedArray.toList.asJava)
                      else null
                    }.asExprOf[T]
                  case "java.util.Set" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
                      if parsedArray != null then new java.util.HashSet(parsedArray.toList.asJava)
                      else null
                    }.asExprOf[T]
                  case "java.util.List" | "java.lang.Iterable" =>
                    '{
                      val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, rtypeRef, in, inTuple) })
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
                            val parsedArray = $in.expectArray[e](() => ${ genReadVal[e](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[e]], in, inTuple) })
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                              () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                              () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                  () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                  () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                                    () => ${ genReadVal[k](ctx, cfg, t.elementRef.asInstanceOf[RTypeRef[k]], in, inTuple, true) },
                                    () => ${ genReadVal[v](ctx, cfg, t.elementRef2.asInstanceOf[RTypeRef[v]], in, inTuple) },
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
                }

          case t: SelfRefRef[?] =>
            println(s"<<< SelfRefRef found: $methodKey >>>")
            val mapExpr = Ref(ctx.readerMapSym).asExprOf[Map[String, JsonSource => Any]] // Symbol for `val readerMap = Map(...)`
            val keyExpr = Expr(t.typedName.toString)
            '{
              $mapExpr($keyExpr).apply($in).asInstanceOf[T]
            }

          case _ =>
            // Classes, traits, etc.
            genDecFnBody[T](ctx, cfg, ref, in) // Create a new decoder function (presumably for class, trait, etc)
            genReadVal(ctx, cfg, ref, in, inTuple)

        // Just-created function is present now and will be called
      )
