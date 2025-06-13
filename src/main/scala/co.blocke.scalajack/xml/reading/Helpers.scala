package co.blocke.scalajack
package xml
package reading

import scala.quoted.*
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters.*
import shared.*

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RTypeRef, TypedName}

sealed trait ReaderEntry
case class Placeholder() extends ReaderEntry
case class RealReader[T](expr: Expr[XmlSource => T], tpe: Type[T]) extends ReaderEntry

object Helpers:

  // Resolved field name will be one of:
  //  * Native field name
  //  * Renamed with @xmlLabel
  //  * Native (or retnamed) class name in the case of @xmlStruct
  // NOTE: If we ever deprecate @xmlStruct then eliminate resolveFieldName and replace with changeFieldName in generateFieldMatrixVal
  private def resolveFieldName(f: FieldInfoRef): String =
    val isStruct = f.annotations.contains("co.blocke.scalajack.xmlStruct")
    f.fieldRef match {
      case c: ClassRef[?] if isStruct =>
//        println("\n-------------------------")
//        println("    Carrier: " + c.name + " field name " + f.name)
//        println(
//          "        Annos: " + c.annotations
//        )
//        println(
//          "        Field Annos: " + f.annotations
//        )
//        println("\n-------------------------")
        c.annotations
          .get("co.blocke.scalajack.xmlLabel")
          .flatMap(_.get("name"))
          .orElse(f.annotations.get("co.blocke.scalajack.xmlLabel").flatMap(_.get("name")))
          .getOrElse(lastPart(c.name))
      case c: CollectionRef[?] if isStruct =>
        c.elementRef match {
          case d: ClassRef[?] =>
            d.annotations
              .get("co.blocke.scalajack.xmlLabel")
              .flatMap(_.get("name"))
              .orElse(f.annotations.get("co.blocke.scalajack.xmlLabel").flatMap(_.get("name")))
              .getOrElse(lastPart(d.name))
          case _ => throw new ParseError(s"Illegal use of @xmlStruct on a field that is not a class (or collection of class)")
        }
      case _ =>
        changeFieldName(f)
    }

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
          if onlyConstructorFields then s.fields.map(f => resolveFieldName(f))
          else (s.fields ++ s.nonConstructorFields.sortBy(_.index)).map(f => resolveFieldName(f))
        case j: JavaClassRef[?] =>
          j.fields.sortBy(_.index).map(f => resolveFieldName(f))
//    println("@@@ Field Matrix Field Names: " + fieldNames)
    val namesArrayExpr = Expr(fieldNames.toArray)
    val matrixExpr = '{
      StringMatrix(if $namesArrayExpr.isEmpty then Array("_") else $namesArrayExpr)
    }
    ValDef(valSym, Some(matrixExpr.asTerm))

  // ----------------------------------------------------------------------
  // Helper functions for types we're generating functions for (keeps main code cleaner)

  /*
  def generateReaderBodyForCaseObjects[T: Type](
                                                 ctx: CodecBuildContext,
                                                 children: List[RTypeRef[?]],
                                                 parentTypeName: String,
                                                 in: Expr[XmlSource]
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
      $in.getFieldValue match {
        case None | Some("") => null
        case Some(v) =>
          ${
            Match(
              '{ $classPrefixExpr + "." + $v }.asTerm,
              caseDefs
            ).asExprOf[T]
          }
      }
    }.asExprOf[T]
   */

  def generateReaderBodyForSealedTraits[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      traitRef: Sealable,
      in: Expr[XmlSource]
  ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    '{ null }.asExprOf[T]
    /*
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
      val unique = Unique.findUniqueWithExcluded(traitRef)
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
     */

  def generateReaderBodyForScalaClass[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      methodKey: TypedName,
      classRef: ScalaClassRef[?],
      in: Expr[XmlSource],
      parentField: Option[FieldInfoRef],
      isStruct: Boolean
  ): Expr[T] =
    if !cfg._writeNonConstructorFields || classRef.nonConstructorFields.isEmpty then generateReaderBodySimple[T](ctx, cfg, classRef, in, parentField, isStruct)
    else Helpers.generateReaderBodyWithNonCtor[T](ctx, cfg, classRef, in, parentField, isStruct)

  private def generateReaderBodySimple[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      in: Expr[XmlSource],
      parentField: Option[FieldInfoRef],
      isStruct: Boolean
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

    val reqRefExpr = Ref(reqSym).asExprOf[Long] // bitmap of fields set so far
    val requiredMaskExpr = Expr(requiredMask) // mask of required fields

//    println(s"^^ Class ${classRef.name} field ${parentField.map(_.name).getOrElse("unknown")}")
    val xmlClassNameE = Expr(
      classRef.annotations
        .get("co.blocke.scalajack.xmlLabel")
        .flatMap(_.get("name"))
        .getOrElse(
          lastPart(classRef.name)
        )
    )

    val parseLogicBody = '{
      // process fields
      var maybeField = $in.expectObjectField
      while maybeField.isDefined do
        maybeField match {
          case Some((fieldName, fieldAttrs)) =>
            val maybeFieldNum = $in.identifyFieldNum(fieldName, $matrixRef)
//            println("Field num: " + maybeFieldNum)
            ${
              Match(
                '{ maybeFieldNum }.asTerm,
                caseDefs :+ CaseDef(Wildcard(), None, '{ $in.skipValue() }.asTerm)
              ).asExprOf[Any]
            }
            if ! $in.expectObjectEnd(fieldName) then throw new ParseError("Element close for label " + fieldName + " not found.")
            maybeField = $in.expectObjectField
          case None => $in.skipValue()
        }
    }

//    println("(parse logic for " + classRef.name + " isStruct? " + isStruct)
    val parseLogic: Term =
      if isStruct then
        '{
          $parseLogicBody
          if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
          else
            throw new ParseError(
              "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
                java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
              )
            )
        }.asTerm
      else
        '{
          val attrs = $in.expectObjectStart($xmlClassNameE)
          // TODO: process attributes
          $parseLogicBody

          if ! $in.expectObjectEnd($xmlClassNameE) then throw new ParseError("Element close for label " + $xmlClassNameE + " not found.")
          if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
          else
            throw new ParseError(
              "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
                java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
              )
            )
        }.asTerm

    Block(fieldMatrixVal +: varDefs :+ reqVarDef, parseLogic).asExprOf[T]

  private def generateReaderBodyWithNonCtor[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      in: Expr[XmlSource],
      parentField: Option[FieldInfoRef],
      isStruct: Boolean
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
              List(Reader.genReadVal[ft](ctx, cfg, f.fieldRef.asInstanceOf[RTypeRef[ft]], in, parentField = parentField, isStruct = isStruct).asTerm)
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

    val xmlClassNameE = Expr(
      classRef.annotations
        .get("co.blocke.scalajack.xmlLabel")
        .flatMap(_.get("name"))
        .getOrElse(
          lastPart(classRef.name)
        )
    )

    val parseLogicBody =
      '{
        var isInstantiated = false
        var maybeField = $in.expectObjectField
        while maybeField.isDefined do
          maybeField match {
            case Some((fieldName, fieldAttrs)) =>
              val maybeFieldNum = $in.identifyFieldNum(fieldName, $matrixRef)
              if maybeFieldNum >= 0 && maybeFieldNum < ${ Expr(classRef.fields.size) } && !isInstantiated then
                ${
                  Match('{ maybeFieldNum }.asTerm, constructorCases).asExprOf[Any]
                }
              else if maybeFieldNum >= 0 && maybeFieldNum < ${ Expr(classRef.fields.size) } && isInstantiated then throw new ParseError("XML elements are ordered. Constructor fields must precede non-constructor fields in order.")
              else if maybeFieldNum < 0 then $in.skipValue() // unknown field ignored
              else
                if !isInstantiated then
                  if ($reqRefExpr & $requiredMaskExpr) == 0 then
                    ${ Assign(Ref(instanceSym), instantiateExpr.asTerm).asExprOf[Unit] }
                    isInstantiated = true
                  else throw new ParseError("Missing required field(s) " + $missingFieldExpr)
                ${
                  Match('{ maybeFieldNum }.asTerm, nonConstructorCases).asExprOf[Any]
                }
            case None =>
              $in.skipValue()
          }
          maybeField = $in.expectObjectField
        ${ Ref(instanceSym).asExprOf[T] }
      }

    val parseLogic: Term =
      if isStruct then
        '{
          $parseLogicBody
          if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
          else
            throw new ParseError(
              "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
                java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
              )
            )
        }.asTerm
      else
        '{
          val attrs = $in.expectObjectStart($xmlClassNameE)
          // TODO: process attributes
          $parseLogicBody

          if ! $in.expectObjectEnd($xmlClassNameE) then throw new ParseError("Element close for label " + $xmlClassNameE + " not found.")
          if ($reqRefExpr & $requiredMaskExpr) == 0 then $instantiateExpr
          else
            throw new ParseError(
              "Missing required field(s) " + ${ Expr(classRef.fields.map(_.name)) }(
                java.lang.Long.numberOfTrailingZeros($reqRefExpr & $requiredMaskExpr)
              )
            )
        }.asTerm

    Block(
      List(fieldMatrixVal) ++ varDefs ++ List(instanceValDef, reqVarDef),
      parseLogic
    ).asExprOf[T]

/*
  def generateReaderBodyForJavaClass[T: Type](
                                               ctx: CodecBuildContext,
                                               cfg: SJConfig,
                                               methodKey: TypedName,
                                               classRef: JavaClassRef[?],
                                               in: Expr[XmlSource]
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
 */
