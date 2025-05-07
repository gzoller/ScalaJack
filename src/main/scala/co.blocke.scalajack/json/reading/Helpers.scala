package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import scala.util.{Failure, Success, Try}
import scala.collection.Factory
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*

import co.blocke.scala_reflection.given
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RType, RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.rtypes.EnumRType

sealed trait ReaderEntry
case class Placeholder() extends ReaderEntry
case class RealReader[T](expr: Expr[JsonSource => T], tpe: Type[T]) extends ReaderEntry

object Helpers:

  private def fieldMatrixExprOf(
      ctx: CodecBuildContext,
      methodKey: TypedName,
      ref: RTypeRef[?]
  ): Option[Expr[StringMatrix]] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*
    ref match
      case _: ScalaClassRef[?] | _: JavaClassRef[?] =>
        ctx.classFieldMatrixSyms.get(methodKey) match
          case Some(sym) =>
            Some(Ref(sym).asExprOf[StringMatrix])
          case None =>
            Some('{ new StringMatrix(Array("_")) }) // <-- if symbol missing, use empty matrix!
      case _ =>
        None

  private def forceFieldMatrix(fieldMatrixOpt: Option[Expr[StringMatrix]])(using Quotes): Expr[StringMatrix] =
    fieldMatrixOpt.getOrElse('{ co.blocke.scalajack.json.StringMatrix(Array("_")) })

  private def prebuildFieldMatrixForClass(
      ctx: CodecBuildContext,
      methodKey: TypedName,
      t: RTypeRef[?],
      onlyConstructorFields: Boolean = true
  ): Unit =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val fieldsArray =
      t match
        case s: ScalaClassRef[?] =>
          if onlyConstructorFields then s.fields.map(f => changeFieldName(f)).toArray
          else (s.fields ++ s.nonConstructorFields.sortBy(_.index)).map(f => changeFieldName(f)).toArray
        case j: JavaClassRef[?] =>
          j.fields.sortBy(_.index).map(f => changeFieldName(f)).toArray

    if fieldsArray.nonEmpty then makeClassFieldMatrixValDef(ctx, methodKey, t.name.replaceAll("\\.", "_"), fieldsArray)
    else
      // If there are no fields at all, still register a dummy empty StringMatrix
      val sym = Symbol.newVal(
        Symbol.spliceOwner,
        "__" + methodKey.toString.replaceAll("\\.", "_") + "_fields",
        TypeRepr.of[StringMatrix],
        Flags.Lazy,
        Symbol.noSymbol
      )
      ctx.classFieldMatrixSyms(methodKey) = sym
      ctx.classFieldMatrixValDefs += (methodKey -> ValDef(sym, Some('{ new StringMatrix(Array.empty[String]) }.asTerm)))

  // This makes a val in the generated code mapping class -> StringMatrix used to rapidly parse fields
  private def makeClassFieldMatrixValDef(
      ctx: CodecBuildContext,
      methodKey: TypedName,
      className: String,
      fieldNames: Array[String]
  ): Unit =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val sym = ctx.classFieldMatrixSyms.getOrElseUpdate(
      methodKey,
      Symbol.newVal(
        Symbol.spliceOwner,
        "__" + className.replaceAll("\\.", "_") + "_fields",
        TypeRepr.of[StringMatrix],
        Flags.Lazy,
        Symbol.noSymbol
      )
    )

    val namesArrayExpr = Varargs(fieldNames.toSeq.map(Expr(_)))
    val namesArray = '{ Array[String]($namesArrayExpr*) }

    // 🧨 ONLY create ValDef and save it -- DO NOT insert it anywhere else yet
    ctx.classFieldMatrixValDefs(methodKey) = ValDef(sym, Some('{ new StringMatrix($namesArray) }.asTerm))

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
      val fieldMatrix = forceFieldMatrix(fieldMatrixExprOf(ctx, methodKey, childRef)).asTerm

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
      val unique = Unique.findUniqueWithExcluded(traitRef)
      val excludeFields = Expr(unique.optionalFields)
      val liftedUnique = liftStringMap(unique.simpleUniqueHash)

      val matchCases: List[CaseDef] = traitRef.sealedChildren.flatMap { classRef =>
        val methodKey = classRef.typedName
        ctx.readMethodSyms.get(methodKey).map { sym =>
          val cond = Literal(StringConstant(classRef.name))
          val fieldMatrix = forceFieldMatrix(fieldMatrixExprOf(ctx, methodKey, classRef)).asTerm
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
      methodKey: TypedName,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    if !cfg._writeNonConstructorFields || classRef.nonConstructorFields.isEmpty then Helpers.generateReaderBodySimple[T](ctx, cfg, methodKey, classRef, in)
    else Helpers.generateReaderBodyWithNonCtor[T](ctx, cfg, methodKey, classRef, in)

  private def generateReaderBodySimple[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      methodKey: TypedName,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix for constructor fields only
    prebuildFieldMatrixForClass(ctx, methodKey, classRef, true)
    val fieldMatrixExpr = Ref(ctx.classFieldMatrixSyms(methodKey)).asExprOf[StringMatrix]

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

    val reqRefExpr = Ref(reqSym).asExprOf[Int]
    val requiredMaskExpr = Expr(requiredMask)

    val parseLogic: Term = '{
      var maybeFieldNum = $in.expectFirstObjectField($fieldMatrixExpr)
      if maybeFieldNum == null then null
      else
        while maybeFieldNum.isDefined do
          ${
            Match(
              '{ maybeFieldNum.get }.asTerm,
              caseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)
            ).asExprOf[Any]
          }
          maybeFieldNum = $in.expectObjectField($fieldMatrixExpr)

        if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
        else
          throw new JsonParseError(
            "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
              Integer.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
            ),
            $in
          )
    }.asTerm

    Block(varDefs :+ reqVarDef, parseLogic).asExprOf[T]

  private def generateReaderBodyWithNonCtor[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      methodKey: TypedName,
      classRef: ScalaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix including constructor + non-constructor fields
    prebuildFieldMatrixForClass(ctx, methodKey, classRef, false)
    val fieldMatrixExpr = Ref(ctx.classFieldMatrixSyms(methodKey)).asExprOf[StringMatrix]

    val (varDefs, idents, reqVarDef, requiredMask, fieldSymbols) =
      FieldDefaultBuilder.generateDefaults[T](ctx, classRef)
    val reqSym = reqVarDef.symbol
    val reqRefExpr = Ref(reqSym).asExprOf[Int]
    val requiredMaskExpr = Expr(requiredMask)

    val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[T], Flags.Mutable, Symbol.noSymbol)
    val instanceValDef = ValDef(instanceSym, Some('{ null }.asTerm))
    val instanceRef = Ref(instanceSym).asExprOf[T]

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
      '{ $ctorFieldNamesExpr(Integer.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)) }

    val parseLogic: Term =
      '{
        val ncBuffer = scala.collection.mutable.ListBuffer.empty[(Int, Int)]
        var maybeFieldNum = $in.expectFirstObjectField($fieldMatrixExpr)

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
            maybeFieldNum = $in.expectObjectField($fieldMatrixExpr)

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

    Block(varDefs ++ List(instanceValDef, reqVarDef), parseLogic).asExprOf[T]

  def generateReaderBodyForJavaClass[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      methodKey: TypedName,
      classRef: JavaClassRef[?],
      in: Expr[JsonSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    // Prebuild matrix including constructor + non-constructor fields
    prebuildFieldMatrixForClass(ctx, methodKey, classRef, false)
    val fieldMatrixExpr = Ref(ctx.classFieldMatrixSyms(methodKey)).asExprOf[StringMatrix]

    classRef.refType match // refType is Type[r.R]
      case '[b] =>
        val classNameE = Expr(classRef.name)
        val tpe = TypeRepr.of[b]
        val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[b], Flags.Mutable, Symbol.noSymbol)
        val instanceSymRef = Ident(instanceSym.termRef)
        val nullConst = tpe.classSymbol.get.companionModule.declaredMethods
          .find(m => m.paramSymss == List(Nil))
          .getOrElse(
            throw JsonTypeError("ScalaJack only supports Java classes that have a zero-argument constructor")
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
            var maybeFieldNum = $in.expectFirstObjectField($fieldMatrixExpr)
            if maybeFieldNum == null then null
            else
              ${ Assign(instanceSymRef, '{ Class.forName($classNameE).getDeclaredConstructor().newInstance().asInstanceOf[b] }.asTerm).asExprOf[Any] } // _instance = (new instance)
              // ${ Assign(instanceSymRef, Apply(Select(New(Inferred(tpe)), nullConst), Nil).asExpr.asTerm).asExprOf[Unit] } // _instance = (new instance)
              while maybeFieldNum.isDefined do
                ${ Match('{ maybeFieldNum.get }.asTerm, caseDefs).asExprOf[Any] }
                maybeFieldNum = $in.expectObjectField($fieldMatrixExpr)

              ${ Ref(instanceSym).asExprOf[Any] }
          }.asTerm
        Block(List(ValDef(instanceSym, Some('{ null }.asTerm))), parseLoop).asExprOf[T]
