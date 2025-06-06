package co.blocke.scalajack

import scala.quoted.*

class SJXmlConfig private[scalajack] (
                                    val noneAsNull: Boolean,
                                    val tryFailureHandling: TryPolicy,
                                    val eitherLeftHandling: EitherLeftPolicy,
                                    // --------------------------
                                    val typeHintLabel: String,
                                    val typeHintPolicy: TypeHintPolicy,
                                    // --------------------------
                                    val enumsAsIds: List[String], // Default: string values.  Nil=all enums as ids, List(...)=specified classes enums as ids
                                    val _writeNonConstructorFields: Boolean,
                                    val _suppressEscapedStrings: Boolean,
                                    val _preferTypeHints: Boolean,

                                    // XML-specific
                                    val _fieldsAsAttributes: Boolean
                                  ):
  def withNoneAsNull: SJXmlConfig = copy(noneAsNull = true)
  def withTryFailureHandling(tryPolicy: TryPolicy): SJXmlConfig = copy(tryFailureHandling = tryPolicy)
  def withEitherLeftHandling(eitherPolicy: EitherLeftPolicy): SJXmlConfig = copy(eitherLeftHandling = eitherPolicy)
  def withTypeHintLabel(label: String): SJXmlConfig = copy(typeHintLabel = label)
  def withTypeHintPolicy(hintPolicy: TypeHintPolicy): SJXmlConfig = copy(typeHintPolicy = hintPolicy)
  def withEnumsAsIds(asIds: List[String]): SJXmlConfig = copy(enumsAsIds = asIds)
  def writeNonConstructorFields: SJXmlConfig = copy(_writeNonConstructorFields = true)
  def suppressEscapedStrings: SJXmlConfig = copy(_suppressEscapedStrings = true)
  def preferTypeHints: SJXmlConfig = copy(_preferTypeHints = true)
  def fieldsAsAttributes: SJXmlConfig = copy(_fieldsAsAttributes = true)

  private def copy(
                    noneAsNull: Boolean = noneAsNull,
                    tryFailureHandling: TryPolicy = tryFailureHandling,
                    eitherLeftHandling: EitherLeftPolicy = eitherLeftHandling,
                    typeHintLabel: String = typeHintLabel,
                    typeHintPolicy: TypeHintPolicy = typeHintPolicy,
                    enumsAsIds: List[String] = enumsAsIds,
                    _writeNonConstructorFields: Boolean = _writeNonConstructorFields,
                    _suppressEscapedStrings: Boolean = _suppressEscapedStrings,
                    _preferTypeHints: Boolean = _preferTypeHints,
                    _fieldsAsAttributes: Boolean = _fieldsAsAttributes
                  ): SJXmlConfig = new SJXmlConfig(
    noneAsNull,
    tryFailureHandling,
    eitherLeftHandling,
    typeHintLabel,
    typeHintPolicy,
    enumsAsIds,
    _writeNonConstructorFields,
    _suppressEscapedStrings,
    _preferTypeHints,
    _fieldsAsAttributes
  )

object SJXmlConfig
  extends SJXmlConfig(
    noneAsNull = false,
    tryFailureHandling = TryPolicy.AS_NULL,
    eitherLeftHandling = EitherLeftPolicy.AS_VALUE,
    typeHintLabel = "_hint",
    typeHintPolicy = TypeHintPolicy.SIMPLE_CLASSNAME,
    enumsAsIds = List("-"), // default -> enum as value
    _writeNonConstructorFields = false,
    _suppressEscapedStrings = false,
    _preferTypeHints = false,
    _fieldsAsAttributes = false
  ):

  private[scalajack] given ToExpr[SJXmlConfig] with {
    def apply(x: SJXmlConfig)(using Quotes): Expr[SJXmlConfig] =
      '{
        val jc = SJXmlConfig
          .withTryFailureHandling(${ Expr(x.tryFailureHandling) })
          .withEitherLeftHandling(${ Expr(x.eitherLeftHandling) })
          .withTypeHintLabel(${ Expr(x.typeHintLabel) })
          .withTypeHintPolicy(${ Expr(x.typeHintPolicy) })
          .withEnumsAsIds(${ Expr(x.enumsAsIds) })
        val jc2 = ${
          if x.noneAsNull then '{ jc.withNoneAsNull }
          else '{ jc }
        }
        val jc3 = ${
          if !x._suppressEscapedStrings then '{ jc2.suppressEscapedStrings }
          else '{ jc2 }
        }
        val jc4 = ${
          if !x._preferTypeHints then '{ jc3.preferTypeHints }
          else '{ jc3 }
        }
        val jc5 = ${
          if !x._writeNonConstructorFields then '{ jc4.writeNonConstructorFields }
          else '{ jc4 }
        }
        val jc6 = ${
          if !x._fieldsAsAttributes then '{ jc5.fieldsAsAttributes }
          else '{ jc5 }
        }
        jc6
      }
  }

  private[scalajack] given FromExpr[SJXmlConfig] with {

    def extract[X: FromExpr](name: String, x: Expr[X])(using Quotes): X =
      import quotes.reflect.*
      summon[FromExpr[X]].unapply(x).getOrElse(throw ConfigError(s"Can't parse $name: ${x.show}, tree: ${x.asTerm}"))

    def unapply(x: Expr[SJXmlConfig])(using Quotes): Option[SJXmlConfig] =
      x match
        case '{
          SJXmlConfig(
            $noneAsNullE,
            $tryFailureHandlerE,
            $eitherLeftHandlerE,
            $typeHintLabelE,
            $typeHintPolicyE,
            $enumsAsIdsE,
            $writeNonConstructorFieldsE,
            $suppressEscapedStringsE,
            $preferTypeHintsE,
            $fieldsAsAttributesE
          )
        } =>
          try
            Some(
              SJXmlConfig(
                extract("noneAsNull", noneAsNullE),
                extract("tryFailureHandler", tryFailureHandlerE),
                extract("eitherLeftHandler", eitherLeftHandlerE),
                extract("typeHintLabel", typeHintLabelE),
                extract("typeHintPolicy", typeHintPolicyE),
                extract("enumsAsIds", enumsAsIdsE),
                extract("_writeNonConstructorFields", writeNonConstructorFieldsE),
                extract("_suppressEscapedStrings", suppressEscapedStringsE),
                extract("_preferTypeHints", preferTypeHintsE),
                extract("_fieldsAsAttributes", fieldsAsAttributesE)
              )
            )
          catch {
            case x =>
              println("ERROR: " + x.getMessage)
              None
          }
        case '{ SJXmlConfig } => Some(SJXmlConfig)
        case '{ ($x: SJXmlConfig).withNoneAsNull } => Some(x.valueOrAbort.withNoneAsNull)
        case '{ ($x: SJXmlConfig).withTryFailureHandling($v) } => Some(x.valueOrAbort.withTryFailureHandling(v.valueOrAbort))
        case '{ ($x: SJXmlConfig).withEitherLeftHandling($v) } => Some(x.valueOrAbort.withEitherLeftHandling(v.valueOrAbort))
        case '{ ($x: SJXmlConfig).withTypeHintLabel($v) } => Some(x.valueOrAbort.withTypeHintLabel(v.valueOrAbort))
        case '{ ($x: SJXmlConfig).withTypeHintPolicy($v) } => Some(x.valueOrAbort.withTypeHintPolicy(v.valueOrAbort))
        case '{ ($x: SJXmlConfig).withEnumsAsIds($v) } => Some(x.valueOrAbort.withEnumsAsIds(v.valueOrAbort))
        case '{ ($x: SJXmlConfig).writeNonConstructorFields } => Some(x.valueOrAbort.writeNonConstructorFields)
        case '{ ($x: SJXmlConfig).suppressEscapedStrings } => Some(x.valueOrAbort.suppressEscapedStrings)
        case '{ ($x: SJXmlConfig).preferTypeHints } => Some(x.valueOrAbort.preferTypeHints)
        case '{ ($x: SJXmlConfig).fieldsAsAttributes } => Some(x.valueOrAbort.fieldsAsAttributes)
  }


  private[scalajack] given ToExpr[TryPolicy] with {
    def apply(x: TryPolicy)(using Quotes): Expr[TryPolicy] =
      x match
        case TryPolicy.AS_NULL => '{ TryPolicy.AS_NULL }
        case TryPolicy.ERR_MSG_STRING => '{ TryPolicy.ERR_MSG_STRING }
        case TryPolicy.THROW_EXCEPTION => '{ TryPolicy.THROW_EXCEPTION }
  }

  private[scalajack] given ToExpr[EitherLeftPolicy] with {
    def apply(x: EitherLeftPolicy)(using Quotes): Expr[EitherLeftPolicy] =
      x match
        case EitherLeftPolicy.AS_VALUE => '{ EitherLeftPolicy.AS_VALUE }
        case EitherLeftPolicy.AS_NULL => '{ EitherLeftPolicy.AS_NULL }
        case EitherLeftPolicy.ERR_MSG_STRING => '{ EitherLeftPolicy.ERR_MSG_STRING }
        case EitherLeftPolicy.THROW_EXCEPTION => '{ EitherLeftPolicy.THROW_EXCEPTION }
  }

  private[scalajack] given ToExpr[TypeHintPolicy] with {
    def apply(x: TypeHintPolicy)(using Quotes): Expr[TypeHintPolicy] =
      x match
        case TypeHintPolicy.SIMPLE_CLASSNAME => '{ TypeHintPolicy.SIMPLE_CLASSNAME }
        case TypeHintPolicy.SCRAMBLE_CLASSNAME => '{ TypeHintPolicy.SCRAMBLE_CLASSNAME }
        case TypeHintPolicy.USE_ANNOTATION => '{ TypeHintPolicy.USE_ANNOTATION }
  }

  private[scalajack] given FromExpr[TryPolicy] with {
    def unapply(x: Expr[TryPolicy])(using Quotes): Option[TryPolicy] =
      x match
        case '{ TryPolicy.AS_NULL } => Some(TryPolicy.AS_NULL)
        case '{ TryPolicy.ERR_MSG_STRING } => Some(TryPolicy.ERR_MSG_STRING)
        case '{ TryPolicy.THROW_EXCEPTION } => Some(TryPolicy.THROW_EXCEPTION)
  }

  private[scalajack] given FromExpr[EitherLeftPolicy] with {
    def unapply(x: Expr[EitherLeftPolicy])(using Quotes): Option[EitherLeftPolicy] =
      x match
        case '{ EitherLeftPolicy.AS_VALUE } => Some(EitherLeftPolicy.AS_VALUE)
        case '{ EitherLeftPolicy.AS_NULL } => Some(EitherLeftPolicy.AS_NULL)
        case '{ EitherLeftPolicy.ERR_MSG_STRING } => Some(EitherLeftPolicy.ERR_MSG_STRING)
        case '{ EitherLeftPolicy.THROW_EXCEPTION } => Some(EitherLeftPolicy.THROW_EXCEPTION)
  }

  private[scalajack] given FromExpr[TypeHintPolicy] with {
    def unapply(x: Expr[TypeHintPolicy])(using Quotes): Option[TypeHintPolicy] =
      x match
        case '{ TypeHintPolicy.SIMPLE_CLASSNAME } => Some(TypeHintPolicy.SIMPLE_CLASSNAME)
        case '{ TypeHintPolicy.SCRAMBLE_CLASSNAME } => Some(TypeHintPolicy.SCRAMBLE_CLASSNAME)
        case '{ TypeHintPolicy.USE_ANNOTATION } => Some(TypeHintPolicy.USE_ANNOTATION)
  }