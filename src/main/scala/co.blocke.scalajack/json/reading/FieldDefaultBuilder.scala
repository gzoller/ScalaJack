package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import scala.util.Success
import co.blocke.scala_reflection.reflect.rtypeRefs.*

/*
	1.	A ValDef for each constructor field (the backing var _fieldname) with an appropriate default value (from Scala’s default param or inferred from the type).
	2.	A Term (Ref) referencing that symbol, used later to instantiate the class.
*/
object FieldDefaultBuilder:

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


  /*
  Step 1 (generateDefaults) is a comprehensive initialization pass where we do the following in one efficient loop over all the fields:
      1.	Create mutable var symbols (_fieldName) to hold each field’s value as it’s parsed.
      2.	Compute default values for each field, including special handling for:
             •	Option types
             •	Java Optionals
             •	Either, Try, etc. (using logic like lrHasOptionChild)
             •	Fallbacks to unitVal and Scala default methods (<init>$default$n)
      3.	Build ValDefs with these default values so we can emit code like var _name = "Greg".
      4.	Accumulate the field references (Ident(sym)) to later feed into the constructor.
      5.	Track a requiredMask bitmask to identify which fields are required (i.e., not optional).
      6.	Create a reqVarDef like var _req = 14 (where 14 means certain fields are required).
      7.	Build a symbol lookup table so later phases like FieldCaseGenerator can assign into _fieldName correctly.
  */
  def generateDefaults[T: Type](
                                 ctx: CodecBuildContext,
                                 classRef: ScalaClassRef[?]
                               ): (List[ctx.quotes.reflect.ValDef], List[ctx.quotes.reflect.Term], ctx.quotes.reflect.ValDef, Int, Map[Int, ctx.quotes.reflect.Symbol]) =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val classSymbol = tpe.typeSymbol
    val classCompanion = Ref(classSymbol.companionModule)
    var requiredMask: Int = 0

    // Extract type arguments (e.g., for generic classes like Foo[A, B])
    // These are used when applying default value methods that are type-parameterized.
    val typeArgList = tpe.typeArgs

    // Map from field index to the corresponding synthetic variable symbol (_fieldName)
    // This lets us look up and reference each var in later phases (like generating CaseDefs).
    val symbolMap = scala.collection.mutable.Map.empty[Int, Symbol]

    def getDefaultValueExpr[f: Type](
                                      field: FieldInfoRef,
                                      idx: Int,
                                      classCompanion: Ref,
                                      typeArgs: List[TypeRepr],
                                      tpe: TypeRepr
                                    ): Expr[f] =
      val dvMembers = classCompanion.tpe.typeSymbol.methodMember(s"<init>$$default$$${idx + 1}")

      if dvMembers.isEmpty then
        field.fieldRef match
          case _: OptionRef[?] | _: AnyRef =>
            field.fieldRef.unitVal.asExprOf[f]

          case r: LeftRightRef[?] if r.lrkind == LRKind.EITHER =>
            val (optionRecipe, lang) = lrHasOptionChild(r)
            if optionRecipe.isEmpty then
              requiredMask |= (1 << idx)
              field.fieldRef.unitVal.asExprOf[f]
            else
              val recipeExpr = Expr(optionRecipe)
              lang match
                case Language.Scala =>
                  '{ $recipeExpr.foldRight(None: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f] }
                case Language.Java =>
                  '{ $recipeExpr.foldRight(java.util.Optional.empty: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f] }

          case r: LeftRightRef[?] =>
            val (optionRecipe, lang) = lrHasOptionChild(r)
            if optionRecipe.isEmpty then
              requiredMask |= (1 << idx)
              field.fieldRef.unitVal.asExprOf[f]
            else if lang == Language.Scala then
              '{ None }.asExprOf[f]
            else
              '{ java.util.Optional.empty.asInstanceOf[f] }

          case y: TryRef[?] =>
            tryHasOptionChild(y) match
              case (true, Language.Scala) => '{ Success(None) }.asExprOf[f]
              case (true, Language.Java)  => '{ Success(java.util.Optional.empty).asInstanceOf[f] }
              case _ =>
                requiredMask |= (1 << idx)
                field.fieldRef.unitVal.asExprOf[f]

          case _ =>
            requiredMask |= (1 << idx)
            field.fieldRef.unitVal.asExprOf[f]

      else
        val methodSymbol = dvMembers.head
        val dvSelectNoTArgs = Select(classCompanion, methodSymbol)
        val applied = methodSymbol.paramSymss match
          case Nil => dvSelectNoTArgs
          case List(params) if params.exists(_.isTypeParam) =>
            if typeArgs.isEmpty then
              throw new JsonTypeError("Expected applied type for: " + tpe.show)
            TypeApply(dvSelectNoTArgs, typeArgs.map(Inferred(_)))
          case _ =>
            throw new JsonTypeError(s"Default method for field `${field.name}` has unsupported parameter shape.")
        applied.asExprOf[f]

    // === Generate field symbols and default ValDefs
    val fieldResults = classRef.fields.zipWithIndex.map { case (field, idx) =>
      field.fieldRef.refType match
        case '[f] =>
          val sym = Symbol.newVal(Symbol.spliceOwner, "_" + field.name, TypeRepr.of[f], Flags.Mutable, Symbol.noSymbol)
          val defaultE = getDefaultValueExpr[f](field, idx, classCompanion, typeArgList, tpe)
          val valDef = ValDef(sym, Some(defaultE.asTerm))
          val ident = Ref(sym)
          symbolMap(idx) = sym
          (valDef, ident)
    }

    val (valDefs, idents) = fieldResults.unzip
    val reqSym = Symbol.newVal(Symbol.spliceOwner, "_req", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
    val reqVarDef = ValDef(reqSym, Some(Literal(IntConstant(requiredMask))))

    (valDefs, idents, reqVarDef, requiredMask, symbolMap.toMap)

