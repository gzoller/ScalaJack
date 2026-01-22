package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import scala.reflect.ClassTag
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef
import shared.*

sealed trait ReaderEntry
case class Placeholder() extends ReaderEntry
case class RealReader[T](expr: Expr[JsonSource => T], tpe: Type[T]) extends ReaderEntry

object Helpers:

  private def generateFieldMatrixVal(
      ctx: CodecBuildContext,
      t: RTypeRef[?],
      onlyConstructorFields: Boolean = true
  ): ctx.quotes.reflect.ValDef =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val valSym = Symbol.newVal(
      Symbol.spliceOwner,
      "__fieldMatrix",
      TypeRepr.of[StringMatrix],
      Flags.EmptyFlags,
      Symbol.noSymbol
    )
    val fieldNames =
      t match
        case s: ScalaClassRef[?] =>
          if onlyConstructorFields then s.fields.map(f => changeFieldName(f))
          else (s.fields ++ s.nonConstructorFields.sortBy(_.index)).map(f => changeFieldName(f))
        case j: JavaClassRef[?] =>
          j.fields.sortBy(_.index).map(f => changeFieldName(f))
    val namesArrayExpr = Expr(fieldNames.toArray)
    val matrixExpr = '{
      StringMatrix(if $namesArrayExpr.isEmpty then Array("_") else $namesArrayExpr)
    }
    ValDef(valSym, Some(matrixExpr.asTerm))

  // ----------------------------------------------------------------------
  // Helper functions for types we're generating functions for (keeps main code cleaner)

  def generateReaderBodyForCaseObjects[T: Type](
      ctx: CodecBuildContext,
      children: List[RTypeRef[?]],
      parentTypeName: String,
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val classPrefix = allButLastPart(parentTypeName) // e.g., co.blocke.MyEnum
    val classPrefixExpr = Expr(classPrefix)

    val caseDefs: List[CaseDef] = children.map { child =>
      child.refType match
        case '[t] =>
          CaseDef(
            Literal(StringConstant(child.name)),
            None,
            Ref(TypeRepr.of[t].typeSymbol).asExprOf[t].asTerm
          )
    }

    '{
      if $in.expectNull() then null
      else
        ${
          Match(
            '{ $classPrefixExpr + "." + $in.expectString() }.asTerm,
            caseDefs
          ).asExprOf[T]
        }
    }.asExprOf[T]

  def generateReaderBodyForSealedTraits[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      traitRef: Sealable,
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val hintLabelE = Expr(cfg.typeHintLabel)
    val named = traitRef match {
      case t: TraitRef[?] => t.name
      case t: ClassRef[?] => t.name // should never happen
      case _              => "unknown" // should never happen
    }
    val tname = Expr(named)
    val classPrefixE = Expr(allButLastPart(named))

    val caseDefs = traitRef.sealedChildren.map { childRef =>
      val childNameE = Expr(childRef.name)
      val methodKey = childRef.typedName

      cfg.typeHintPolicy match
        case TypeHintPolicy.SCRAMBLE_CLASSNAME =>
          val sym = Symbol.newBind(Symbol.spliceOwner, "t", Flags.EmptyFlags, TypeRepr.of[String])
          CaseDef(
            Bind(sym, Typed(Wildcard(), Inferred(TypeRepr.of[String]))),
            Some({
              val tE = Ref(sym).asExprOf[String]
              '{ descramble($tE, lastPart($childNameE).hashCode) }.asTerm
            }),
            ctx.readMethodSyms
              .get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
              }
              .get
          )
        case TypeHintPolicy.USE_ANNOTATION =>
          val annoOrName = childRef match
            case cr: ClassRef[?] =>
              cr.annotations
                .get("co.blocke.scalajack.TypeHint")
                .flatMap(_.get("hintValue"))
                .getOrElse(lastPart(cr.name))
            case _ => lastPart(childRef.name)
          CaseDef(
            Literal(StringConstant(annoOrName)),
            None,
            ctx.readMethodSyms
              .get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
              }
              .get
          )

        case TypeHintPolicy.SIMPLE_CLASSNAME =>
          CaseDef(
            Literal(StringConstant(childRef.name)),
            None,
            ctx.readMethodSyms
              .get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
              }
              .get
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
      val unique = Unique.findUniqueWithExcluded(traitRef)(ctx)
      val excludeFields = Expr(unique.optionalFields)
      val liftedUnique = liftStringMap(unique.simpleUniqueHash)

      val matchCases: List[CaseDef] = traitRef.sealedChildren.flatMap { classRef =>
        val methodKey = classRef.typedName
        ctx.readMethodSyms.get(methodKey).map { sym =>
          val cond = Literal(StringConstant(classRef.name))
          val rhs = Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
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

  def generateReaderBodyForScalaClass[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    if !cfg._writeNonConstructorFields || classRef.nonConstructorFields.isEmpty then Helpers.generateReaderBodySimple[T](ctx, cfg, classRef, in)
    else Helpers.generateReaderBodyWithNonCtor[T](ctx, cfg, classRef, in)

  private def generateReaderBodySimple[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix for constructor fields only
    val fieldMatrixVal = generateFieldMatrixVal(ctx, classRef)
    val matrixRef = Ref(fieldMatrixVal.symbol).asExprOf[StringMatrix]

    val (varDefs, idents, reqVarDef, requiredMask, fieldSymbols) =
      FieldDefaultBuilder.generateDefaults[T](ctx, classRef)
    val reqSym = reqVarDef.symbol

    val caseDefs = FieldCaseGenerator.generateConstructorFieldCases(
      ctx,
      cfg,
      classRef,
      reqSym,
      fieldSymbols,
      in
    )

    val instantiateExpr = ConstructorBuilder
      .buildClassInstantiationExpr(ctx, TypeRepr.of[T], idents)
      .asExprOf[T]

    val reqRefExpr = Ref(reqSym).asExprOf[Long]
    val requiredMaskExpr = Expr(requiredMask)

    val parseLogic: Term = '{
      var maybeFieldNum = $in.expectFirstObjectField($matrixRef)
      if maybeFieldNum == null then null
      else
        while maybeFieldNum.isDefined do
          ${
            Match(
              '{ maybeFieldNum.get }.asTerm,
              caseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)
            ).asExprOf[Any]
          }
          maybeFieldNum = $in.expectObjectField($matrixRef)

        if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
        else
          throw new JsonParseError(
            "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
              java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
            ),
            $in
          )
    }.asTerm

    Block(fieldMatrixVal +: varDefs :+ reqVarDef, parseLogic).asExprOf[T]

  private def generateReaderBodyWithNonCtor[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix including constructor + non-constructor fields
    val fieldMatrixVal = generateFieldMatrixVal(ctx, classRef, false)
    val matrixRef = Ref(fieldMatrixVal.symbol).asExprOf[StringMatrix]

    val (varDefs, idents, reqVarDef, requiredMask, fieldSymbols) =
      FieldDefaultBuilder.generateDefaults[T](ctx, classRef)
    val reqSym = reqVarDef.symbol
    val reqRefExpr = Ref(reqSym).asExprOf[Long]
    val requiredMaskExpr = Expr(requiredMask)

    val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[T], Flags.Mutable, Symbol.noSymbol)
    val instanceValDef = ValDef(instanceSym, Some('{ null }.asTerm))

    val constructorCaseDefs = FieldCaseGenerator.generateConstructorFieldCases(
      ctx,
      cfg,
      classRef,
      reqSym,
      fieldSymbols,
      in
    )

    val nonCtorCases: List[CaseDef] = classRef.nonConstructorFields.map { f =>
      f.fieldRef.refType match
        case '[ft] =>
          CaseDef(
            Literal(IntConstant(f.index + classRef.fields.size)),
            None,
            Apply(
              Select.unique(Ref(instanceSym), f.setterLabel),
              List(Reader.genReadVal[ft](ctx, cfg, f.fieldRef.asInstanceOf[RTypeRef[ft]], in).asTerm)
            )
          )
    }

    val constructorCases = constructorCaseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)
    val nonConstructorCases = nonCtorCases :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)

    val instantiateExpr = ConstructorBuilder
      .buildClassInstantiationExpr(ctx, TypeRepr.of[T], idents)
      .asExprOf[T]

    val ctorFieldNamesExpr: Expr[List[String]] =
      Expr(classRef.fields.map(_.name))

    val missingFieldExpr =
      '{ $ctorFieldNamesExpr(java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)) }

    val parseLogic: Term =
      '{
        val ncBuffer = scala.collection.mutable.ListBuffer.empty[(Int, Int)]
        var maybeFieldNum = $in.expectFirstObjectField($matrixRef)

        if maybeFieldNum == null then null.asInstanceOf[T]
        else
          while maybeFieldNum.isDefined do
            val foundFieldNum = maybeFieldNum.get
            if foundFieldNum < ${ Expr(classRef.fields.size) } then
              ${
                Match('{ foundFieldNum }.asTerm, constructorCases).asExprOf[Any]
              }
            else {
              ncBuffer += ((foundFieldNum, $in.pos))
              $in.skipValue()
            }
            maybeFieldNum = $in.expectObjectField($matrixRef)

          if ($reqRefExpr & $requiredMaskExpr) == 0 then
            ${ Assign(Ref(instanceSym), instantiateExpr.asTerm).asExprOf[Unit] }

            ncBuffer.foreach { case (fieldNum, pos) =>
              $in.revertToPos(pos)
              ${
                Match('{ fieldNum }.asTerm, nonConstructorCases).asExprOf[Any]
              }
            }

            ${ Ref(instanceSym).asExprOf[T] }
          else throw new JsonParseError("Missing required field(s) " + $missingFieldExpr, $in)
      }.asTerm

    Block(
      List(fieldMatrixVal) ++ varDefs ++ List(instanceValDef, reqVarDef),
      parseLogic
    ).asExprOf[T]

  def generateReaderBodyForJavaClass[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: JavaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix including constructor + non-constructor fields
    val fieldMatrixVal = generateFieldMatrixVal(ctx, classRef, false)
    val matrixRef = Ref(fieldMatrixVal.symbol).asExprOf[StringMatrix]

    classRef.refType match // refType is Type[r.R]
      case '[b] =>
        val classNameE = Expr(classRef.name)
        val tpe = TypeRepr.of[b]
        val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[b], Flags.Mutable, Symbol.noSymbol)
        val instanceSymRef = Ident(instanceSym.termRef)
        tpe.classSymbol.get.companionModule.declaredMethods
          .find(m => m.paramSymss == List(Nil))
          .getOrElse(
            throw TypeError("ScalaJack only supports Java classes that have a zero-argument constructor")
          )
        val caseDefs = classRef.fields.map(ncf =>
          ncf.fieldRef.refType match
            case '[u] =>
              CaseDef(
                Literal(IntConstant(ncf.index)),
                None,
                // Call the setter for this field here...
                Apply(
                  Select.unique(Ref(instanceSym), ncf.asInstanceOf[NonConstructorFieldInfoRef].setterLabel),
                  List(Reader.genReadVal[u](ctx, cfg, ncf.fieldRef.asInstanceOf[RTypeRef[u]], in).asTerm)
                ).asExpr.asTerm
              )
        ) :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm) // skip values of unrecognized fields

        val parseLoop =
          '{
            var maybeFieldNum = $in.expectFirstObjectField($matrixRef)
            if maybeFieldNum == null then null
            else
              ${ Assign(instanceSymRef, '{ Class.forName($classNameE).getDeclaredConstructor().newInstance().asInstanceOf[b] }.asTerm).asExprOf[Any] } // _instance = (new instance)
              while maybeFieldNum.isDefined do
                ${ Match('{ maybeFieldNum.get }.asTerm, caseDefs).asExprOf[Any] }
                maybeFieldNum = $in.expectObjectField($matrixRef)

              ${ Ref(instanceSym).asExprOf[Any] }
          }.asTerm
        Block(
          List(
            fieldMatrixVal,
            ValDef(instanceSym, Some('{ null }.asTerm))
          ),
          parseLoop
        ).asExprOf[T]
