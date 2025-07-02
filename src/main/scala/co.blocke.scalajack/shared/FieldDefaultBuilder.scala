package co.blocke.scalajack
package shared

import co.blocke.scala_reflection.Language
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scalajack.TypeError

import scala.quoted.*
import scala.util.Success

/*
	1.	A ValDef for each constructor field (the backing var _fieldname) with an appropriate default value (from Scala’s default param or inferred from the type).
	2.	A Term (Ref) referencing that symbol, used later to instantiate the class.
 */
object FieldDefaultBuilder:

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
  ): (List[ctx.quotes.reflect.ValDef], List[ctx.quotes.reflect.Term], ctx.quotes.reflect.ValDef, Long, Map[Int, ctx.quotes.reflect.Symbol]) =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val classSymbol = tpe.typeSymbol
    val classCompanion = Ref(classSymbol.companionModule)
    var requiredMask: Long = 0

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
            val (optionRecipe, lang) = r.hasOptionChild match {
              case Some(or, l) => (or, l)
              case None        => ("", Language.Scala)
            }
            if optionRecipe.isEmpty then
              requiredMask |= (1L << idx)
              field.fieldRef.unitVal.asExprOf[f]
            else
              val recipeExpr = Expr(optionRecipe)
              lang match
                case Language.Scala =>
                  '{ $recipeExpr.foldRight(None: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f] }
                case Language.Java =>
                  '{ $recipeExpr.foldRight(java.util.Optional.empty: Any)((c, acc) => if c == 'r' then Right(acc) else Left(acc)).asInstanceOf[f] }

          case r: LeftRightRef[?] =>
            val (optionRecipe, lang) = r.hasOptionChild match {
              case Some(or, l) => (or, l)
              case None        => ("", Language.Scala)
            }
            if optionRecipe.isEmpty then
              requiredMask |= (1L << idx)
              field.fieldRef.unitVal.asExprOf[f]
            else if lang == Language.Scala then '{ None }.asExprOf[f]
            else '{ java.util.Optional.empty.asInstanceOf[f] }

          case y: TryRef[?] =>
            y.hasOptionChild match {
              case Some(Language.Scala) => '{ Success(None) }.asExprOf[f]
              case Some(Language.Java)  => '{ Success(java.util.Optional.empty).asInstanceOf[f] }
              case _ =>
                requiredMask |= (1L << idx)
                field.fieldRef.unitVal.asExprOf[f]
            }

          case a: AliasRef[?] =>
            requiredMask |= (1L << idx)
            Expr.summon[JsonDefault[f]] match
              case Some(defaultExpr) =>
                '{ $defaultExpr.default }.asExprOf[f] // safe Expr[T]
              case None =>
                throw new TypeError(s"No default value found for type ${a.name}. Consider providing a given JsonDefault.")

          case _ =>
            requiredMask |= (1L << idx)
            field.fieldRef.unitVal.asExprOf[f]
      else
        val methodSymbol = dvMembers.head
        val dvSelectNoTArgs = Select(classCompanion, methodSymbol)
        val applied = methodSymbol.paramSymss match
          case Nil => dvSelectNoTArgs
          case List(params) if params.exists(_.isTypeParam) =>
            if typeArgs.isEmpty then throw new TypeError("Expected applied type for: " + tpe.show)
            TypeApply(dvSelectNoTArgs, typeArgs.map(Inferred(_)))
          case _ =>
            throw new TypeError(s"Default method for field `${field.name}` has unsupported parameter shape.")
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
    val reqSym = Symbol.newVal(Symbol.spliceOwner, "_req", TypeRepr.of[Long], Flags.Mutable, Symbol.noSymbol)
    val reqVarDef = ValDef(reqSym, Some(Literal(LongConstant(requiredMask))))

    (valDefs, idents, reqVarDef, requiredMask, symbolMap.toMap)
