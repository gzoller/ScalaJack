package co.blocke.scalajack.shared

object ConstructorBuilder:
  def buildClassInstantiationExpr(
      ctx: CodecBuildContext,
      tpe: ctx.quotes.reflect.TypeRepr,
      fieldArgs: List[ctx.quotes.reflect.Term]
  ): ctx.quotes.reflect.Term =
    import ctx.quotes.reflect.*

    // Singleton cases of an enum with payload cases are decoded as objects too.
    tpe match
      case singleton: TermRef if singleton.termSymbol.flags.is(Flags.Enum) =>
        return Ref(singleton.termSymbol)
      case _ => ()

    val classSymbol = tpe.classSymbol.get
    val primaryConstructor = classSymbol.primaryConstructor
    val newExpr = New(Inferred(tpe))

    // Apply type arguments if needed
    val constructorSelect = Select(newExpr, primaryConstructor)

    val appliedConstructor =
      tpe match
        case AppliedType(_, typeArgs) if typeArgs.nonEmpty =>
          TypeApply(constructorSelect, typeArgs.map(Inferred(_)))
        case _ =>
          constructorSelect

    // Now apply the arguments
    Apply(appliedConstructor, fieldArgs)
