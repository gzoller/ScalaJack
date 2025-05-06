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


object MakeReadFn:

  private def lrHasOptionChild(lr: LeftRightRef[?]): (String, Language) =
    lr.rightRef match
      case t: ScalaOptionRef[?] => ("r", Language.Scala)
      case t: JavaOptionalRef[?] => ("r", Language.Java)
      case t: LeftRightRef[?] =>
        val (recipe, lang) = lrHasOptionChild(t)
        ("r" + recipe, lang)
      case _ =>
        lr.leftRef match
          case t: ScalaOptionRef[?] => ("l", Language.Scala)
          case t: JavaOptionalRef[?] => ("l", Language.Java)
          case t: LeftRightRef[?] =>
            val (recipe, lang) = lrHasOptionChild(t)
            ("l" + recipe, lang)
          case _ => ("", Language.Scala)

  private def tryHasOptionChild(t: TryRef[?]): (Boolean, Language) =
    t.tryRef match
      case o: ScalaOptionRef[?] => (true, Language.Scala)
      case o: JavaOptionalRef[?] => (true, Language.Java)
      case lr: LeftRightRef[?] =>
        val (recipe, lang) = lrHasOptionChild(lr)
        (recipe.nonEmpty, lang)
      case _ => (false, Language.Scala)


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


  def prebuildFieldMatrixForClass(
                                           ctx: CodecBuildContext,
                                           cfg: SJConfig,
                                           methodKey: TypedName,
                                           t: RTypeRef[?]
                                         ): Unit =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val fieldsArray =
      t match
        case s: ScalaClassRef[?] =>
          if !cfg._writeNonConstructorFields || s.nonConstructorFields.isEmpty then s.fields.map(f => changeFieldName(f)).toArray
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
        Flags.Private,
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
        Flags.Private,
        Symbol.noSymbol
      )
    )

    val namesArrayExpr = Varargs(fieldNames.map(Expr(_)))
    val namesArray = '{ Array[String]($namesArrayExpr *) }

    // 🧨 ONLY create ValDef and save it -- DO NOT insert it anywhere else yet
    ctx.classFieldMatrixValDefs(methodKey) = ValDef(sym, Some('{ new StringMatrix($namesArray) }.asTerm))


  //----------------------------------------------------------------------
// Helper functions for types we're generating functions for (keeps main code cleaner)


  def generateReaderBodyForCaseObjects[T: Type](
                                        ctx: CodecBuildContext,
                                        children: List[RTypeRef[?]],
                                        parentTypeName: String
                                      ): Expr[(JsonSource) => T] =
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
      (in: JsonSource) =>
        if in.expectNull() then null
        else ${
          Match(
            '{ $classPrefixExpr + "." + in.expectString() }.asTerm,
            caseDefs
          ).asExprOf[T]
        }
    }.asExprOf[JsonSource => T]


  def generateReaderBodyForSealedTraits[T: Type](
                                                 ctx: CodecBuildContext,
                                                 cfg: SJConfig,
                                                 traitRef: Sealable,
                                                 in: Expr[JsonSource]
                                               ): Expr[(JsonSource) => T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val hintLabelE = Expr(cfg.typeHintLabel)
    val named = traitRef match {
      case t: TraitRef[?] => t.name
      case t: ClassRef[?] => t.name // should never happen
      case _ => "unknown"  // should never happen
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
            ctx.readMethodSyms.get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
                }
              .get
          )
        case TypeHintPolicy.USE_ANNOTATION =>
          val annoOrName = childRef match
            case cr: ClassRef[?] =>
              cr.annotations.get("co.blocke.scalajack.TypeHint")
                .flatMap(_.get("hintValue"))
                .getOrElse(lastPart(cr.name))
            case _ => lastPart(childRef.name)
          CaseDef(
            Literal(StringConstant(annoOrName)),
            None,
            ctx.readMethodSyms.get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
              }
              .get
          )

        case TypeHintPolicy.SIMPLE_CLASSNAME =>
          CaseDef(
            Literal(StringConstant(childRef.name)),
            None,
            ctx.readMethodSyms.get(methodKey)
              .map { sym =>
                Apply(Ref(sym), List(in.asTerm)).asExprOf[Any].asTerm
              }
              .get
          )
    }

    if cfg._preferTypeHints then
      '{
        (in: JsonSource) =>
          if in.expectNull() then null
          else
            val hint = in.findObjectField($hintLabelE).getOrElse(throw JsonParseError(s"Unable to find type hint for abstract class $$cnameE", in))
            ${
              cfg.typeHintPolicy match
                case TypeHintPolicy.SIMPLE_CLASSNAME => Match('{ $classPrefixE + "." + hint }.asTerm, caseDefs).asExprOf[T]
                case TypeHintPolicy.SCRAMBLE_CLASSNAME => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                case TypeHintPolicy.USE_ANNOTATION => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
            }
      }.asExprOf[JsonSource => T]
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
        (in: JsonSource) =>
          if in.expectNull() then null
          else {
            // 1. See if type hint is present--if so, use it!
            in.findObjectField($hintLabelE) match {
              case Some(hint) =>
                ${
                  cfg.typeHintPolicy match
                    case TypeHintPolicy.SIMPLE_CLASSNAME => Match('{ $classPrefixE + "." + hint }.asTerm, caseDefs).asExprOf[T]
                    case TypeHintPolicy.SCRAMBLE_CLASSNAME => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                    case TypeHintPolicy.USE_ANNOTATION => Match('{ hint }.asTerm, caseDefs).asExprOf[T]
                }
              case None =>
                // 2. Find all field names
                val fields = in.findAllFieldNames()
                // 3. Strip out all excluded (optional) fields and make hash
                val fingerprint = Unique.hashOf(fields.filterNot($excludeFields.contains))
                // 4. Look up hash
                val className = $liftedUnique
                  .get(fingerprint)
                  .getOrElse(
                    // Before admitting failure, it is possible "" is a valid hash key!
                    $liftedUnique.get("").getOrElse(throw new JsonParseError("Class in trait " + $tname + s" with parsed fields [${fields.mkString(",")}] needed a type hint but none was found (ambiguous)", in))
                  )
                ${ Match('{ className }.asTerm, matchCases).asExprOf[T] }
            }
          }
      }.asExprOf[JsonSource => T]


  def generateReaderBodyForScalaClass[T: Type](
                                              ctx: CodecBuildContext,
                                              cfg: SJConfig,
                                              methodKey: TypedName,
                                              classRef: ScalaClassRef[?],
                                              in: Expr[JsonSource],
                                              fieldMatrixExpr: Expr[StringMatrix]
                                       ): Expr[(JsonSource) => T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    inline def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
      case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
      case _ => Nil

    classRef.refType match // refType is Type[r.R]
      case '[b] =>
        // Generate vars for each contractor argument, populated with either a "unit" value (eg 0, "") or given default value
        val tpe = TypeRepr.of[b]
        val classCompanion = tpe.typeSymbol.companionClass
        val companionModule = tpe.typeSymbol.companionModule
        val totalRequired = math.pow(2, classRef.fields.length).toInt - 1
        var required = 0
        val reqSym = Symbol.newVal(Symbol.spliceOwner, "required", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
        val allFieldNames = Expr(classRef.fields.map(f => changeFieldName(f)).toArray) // Used for missing required field error

        val together = classRef.fields.map { oneField =>
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
                    ${ Assign(fieldSymRef, Reader.genReadVal[f](ctx, cfg, oneField.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit] }
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
                      case (true, Language.Java) => '{ Success(java.util.Optional.empty).asInstanceOf[f] }.asTerm
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
                      case Nil => throw JsonTypeError("Expected an applied type for "+tpe)
                      case typeArgs => TypeApply(dvSelectNoTArgs, typeArgs.map(Inferred(_)))
                  case _ => throw JsonTypeError(s"Default constructor parameter method `${methodSymbol.name}` in class ${tpe.show} has unsupported parameter structure. " +
                    "ScalaJack only supports simple default methods with no or simple type parameters.")
                (ValDef(sym, Some(dvSelect)), caseDef, fieldSymRef)
          }
        }
        val reqVarDef = ValDef(reqSym, Some(Literal(IntConstant(totalRequired))))
        val (varDefs, constructorFieldCaseDefs, idents) = together.unzip3

        val argss = List(idents)
        val primaryConstructor = tpe.classSymbol.get.primaryConstructor
        val constructorNoTypes = Select(New(Inferred(tpe)), primaryConstructor)
        val constructor = typeArgs(tpe) match
          case Nil => constructorNoTypes
          case typeArgs => TypeApply(constructorNoTypes, typeArgs.map(Inferred(_)))
        val instantiateClass = argss.tail.foldLeft(Apply(constructor, argss.head))((acc, args) => Apply(acc, args))

        val exprRequired = Expr(required)
        var finalVarDefs = varDefs
        val parseLoop: Expr[JsonSource => T] =
          '{
            (in: JsonSource) =>
              ${
                if !cfg._writeNonConstructorFields || classRef.nonConstructorFields.isEmpty then
                  // ------- SIMPLE CONSTRUCTOR-ONLY CASE -------
                  Block(
                    finalVarDefs :+ reqVarDef,
                    '{
                      var maybeFieldNum = in.expectFirstObjectField($fieldMatrixExpr)
                      if maybeFieldNum == null then null
                      else
                        while maybeFieldNum.isDefined do
                          ${ Match('{ maybeFieldNum.get }.asTerm,
                            constructorFieldCaseDefs :+
                              CaseDef(Wildcard(), None, '{ in.skipValue() }.asTerm)
                          ).asExprOf[Any] }
                          maybeFieldNum = in.expectObjectField($fieldMatrixExpr)

                        if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then
                          ${ instantiateClass.asExprOf[T] }
                        else
                          throw new JsonParseError(
                            "Missing required field(s) " +
                              ${ allFieldNames }(
                            Integer.numberOfTrailingZeros(
                              ${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }
                            )
                          ) ,
                      in
                      )
                    }.asTerm
                  ).asExprOf[T]

                else
                  // ------- CONSTRUCTOR + FINAL (NON-CONSTRUCTOR) FIELDS -------
                  val instanceSym = Symbol.newVal(Symbol.spliceOwner, "_instance", TypeRepr.of[b], Flags.Mutable, Symbol.noSymbol)
                  finalVarDefs = finalVarDefs :+ ValDef(instanceSym, Some('{ null }.asTerm))
                  val instanceSymRef = Ident(instanceSym.termRef)

                  val caseDefsWithFinalNC = constructorFieldCaseDefs ++
                    classRef.nonConstructorFields.map(ncf =>
                      ncf.fieldRef.refType match
                        case '[u] =>
                          CaseDef(
                            Literal(IntConstant(ncf.index + classRef.fields.size)),
                            None,
                            Apply(
                              Select.unique(Ref(instanceSym), ncf.setterLabel),
                              List(
                                Reader.genReadVal[u](
                                  ctx,
                                  cfg,
                                  ncf.fieldRef.asInstanceOf[RTypeRef[u]],
                                  'in
                                ).asTerm
                              )
                            ).asExpr.asTerm
                          )
                    ) :+ CaseDef(Wildcard(), None, '{ in.skipValue() }.asTerm)

                  val numCtorFields = Expr(classRef.fields.size)

                  Block(
                    finalVarDefs :+ reqVarDef,
                    '{
                      val ncBuffer = scala.collection.mutable.ListBuffer.empty[(Int, Int)]
                      var maybeFieldNum = in.expectFirstObjectField($fieldMatrixExpr)
                      if maybeFieldNum == null then null
                      else
                        while maybeFieldNum.isDefined do
                          val fieldNum = maybeFieldNum.get
                          if fieldNum < $numCtorFields then
                            ${ Match('{ fieldNum }.asTerm, caseDefsWithFinalNC).asExprOf[Any] }
                          else {
                            ncBuffer += (fieldNum -> in.pos)
                            in.skipValue()
                          }
                          maybeFieldNum = in.expectObjectField($fieldMatrixExpr)

                        if (${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }) == 0 then
                          ${ Assign(instanceSymRef, instantiateClass.asExpr.asTerm).asExprOf[Unit] }

                          ncBuffer.foreach { case (fieldNum, pos) =>
                            in.revertToPos(pos)
                            ${ Match('{ fieldNum }.asTerm, caseDefsWithFinalNC).asExprOf[Any] }
                          }
                        else
                          throw new JsonParseError(
                            "Missing required field(s) " +
                              ${ allFieldNames }(
                            Integer.numberOfTrailingZeros(
                              ${ Ref(reqSym).asExprOf[Int] } & ${ exprRequired }
                            )
                          ) ,
                      in
                      )
                      ${ Ref(instanceSym).asExprOf[T] }
                    }.asTerm
                  ).asExprOf[T]
              }
          }
        Block(finalVarDefs :+ reqVarDef, parseLoop.asTerm).asExprOf[JsonSource=>T]

  
  def generateReaderBodyForJavaClass[T: Type](
                                               ctx: CodecBuildContext,
                                               cfg: SJConfig,
                                               methodKey: TypedName,
                                               classRef: JavaClassRef[?],
                                               in: Expr[JsonSource],
                                               fieldMatrixExpr: Expr[StringMatrix]
                                             ): Expr[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

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
                  List(Reader.genReadVal[u](ctx, cfg, ncf.fieldRef.asInstanceOf[RTypeRef[u]], in).asExprOf[b].asTerm)
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
