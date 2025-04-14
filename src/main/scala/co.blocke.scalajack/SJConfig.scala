package co.blocke.scalajack

import co.blocke.scala_reflection.TypedName
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*
import scala.util.control.NoStackTrace

class ConfigError(msg: String) extends Throwable(msg) with NoStackTrace

class SJConfig private[scalajack] (
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
    val _preferTypeHints: Boolean
):
  def withNoneAsNull: SJConfig = copy(noneAsNull = true)
  def withTryFailureHandling(tryPolicy: TryPolicy): SJConfig = copy(tryFailureHandling = tryPolicy)
  def withEitherLeftHandling(eitherPolicy: EitherLeftPolicy): SJConfig = copy(eitherLeftHandling = eitherPolicy)
  def withTypeHintLabel(label: String): SJConfig = copy(typeHintLabel = label)
  def withTypeHintPolicy(hintPolicy: TypeHintPolicy): SJConfig = copy(typeHintPolicy = hintPolicy)
  def withEnumsAsIds(asIds: List[String]): SJConfig = copy(enumsAsIds = asIds)
  def writeNonConstructorFields: SJConfig = copy(_writeNonConstructorFields = true)
  def suppressEscapedStrings: SJConfig = copy(_suppressEscapedStrings = true)
  def preferTypeHints: SJConfig = copy(_preferTypeHints = true)

  private def copy(
      noneAsNull: Boolean = noneAsNull,
      tryFailureHandling: TryPolicy = tryFailureHandling,
      eitherLeftHandling: EitherLeftPolicy = eitherLeftHandling,
      typeHintLabel: String = typeHintLabel,
      typeHintPolicy: TypeHintPolicy = typeHintPolicy,
      enumsAsIds: List[String] = enumsAsIds,
      _writeNonConstructorFields: Boolean = _writeNonConstructorFields,
      _suppressEscapedStrings: Boolean = _suppressEscapedStrings,
      _preferTypeHints: Boolean = _preferTypeHints
  ): SJConfig = new SJConfig(
    noneAsNull,
    tryFailureHandling,
    eitherLeftHandling,
    typeHintLabel,
    typeHintPolicy,
    enumsAsIds,
    _writeNonConstructorFields,
    _suppressEscapedStrings,
    _preferTypeHints
  )

enum TryPolicy:
  case AS_NULL, ERR_MSG_STRING, THROW_EXCEPTION

enum EitherLeftPolicy:
  case AS_VALUE, AS_NULL, ERR_MSG_STRING, THROW_EXCEPTION

enum TypeHintPolicy:
  case SIMPLE_CLASSNAME, SCRAMBLE_CLASSNAME, USE_ANNOTATION

object SJConfig
    extends SJConfig(
      noneAsNull = false,
      tryFailureHandling = TryPolicy.AS_NULL,
      eitherLeftHandling = EitherLeftPolicy.AS_VALUE,
      typeHintLabel = "_hint",
      typeHintPolicy = TypeHintPolicy.SIMPLE_CLASSNAME,
      enumsAsIds = List("-"), // default -> enum as value
      _writeNonConstructorFields = false,
      _suppressEscapedStrings = false,
      _preferTypeHints = false
    ):
  import scala.quoted.FromExpr.*

  private[scalajack] given ToExpr[SJConfig] with {
    def apply(x: SJConfig)(using Quotes): Expr[SJConfig] =
      '{
        val jc = SJConfig
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
        jc5
      }
  }

  private[scalajack] given FromExpr[SJConfig] with {

    def extract[X: FromExpr](name: String, x: Expr[X])(using Quotes): X =
      import quotes.reflect.*
      summon[FromExpr[X]].unapply(x).getOrElse(throw ConfigError(s"Can't parse $name: ${x.show}, tree: ${x.asTerm}"))

    def unapply(x: Expr[SJConfig])(using Quotes): Option[SJConfig] =
      import quotes.reflect.*

      x match
        case '{
              SJConfig(
                $noneAsNullE,
                $tryFailureHandlerE,
                $eitherLeftHandlerE,
                // $undefinedFieldHandlingE,
                $typeHintLabelE,
                $typeHintPolicyE,
                $enumsAsIdsE,
                $writeNonConstructorFieldsE,
                $suppressEscapedStringsE,
                $preferTypeHintsE
              )
            } =>
          try
            Some(
              SJConfig(
                extract("noneAsNull", noneAsNullE),
                extract("tryFailureHandler", tryFailureHandlerE),
                extract("eitherLeftHandler", eitherLeftHandlerE),
                extract("typeHintLabel", typeHintLabelE),
                extract("typeHintPolicy", typeHintPolicyE),
                extract("enumsAsIds", enumsAsIdsE),
                extract("_writeNonConstructorFields", writeNonConstructorFieldsE),
                extract("_suppressEscapedStrings", suppressEscapedStringsE),
                extract("_preferTypeHints", preferTypeHintsE)
              )
            )
          catch {
            case x =>
              println("ERROR: " + x.getMessage)
              None
          }
        case '{ SJConfig }                                  => Some(SJConfig)
        case '{ ($x: SJConfig).withNoneAsNull }             => Some(x.valueOrAbort.withNoneAsNull)
        case '{ ($x: SJConfig).withTryFailureHandling($v) } => Some(x.valueOrAbort.withTryFailureHandling(v.valueOrAbort))
        case '{ ($x: SJConfig).withEitherLeftHandling($v) } => Some(x.valueOrAbort.withEitherLeftHandling(v.valueOrAbort))
        case '{ ($x: SJConfig).withTypeHintLabel($v) }      => Some(x.valueOrAbort.withTypeHintLabel(v.valueOrAbort))
        case '{ ($x: SJConfig).withTypeHintPolicy($v) }     => Some(x.valueOrAbort.withTypeHintPolicy(v.valueOrAbort))
        case '{ ($x: SJConfig).withEnumsAsIds($v) }         => Some(x.valueOrAbort.withEnumsAsIds(v.valueOrAbort))
        case '{ ($x: SJConfig).writeNonConstructorFields }  => Some(x.valueOrAbort.writeNonConstructorFields)
        case '{ ($x: SJConfig).suppressEscapedStrings }     => Some(x.valueOrAbort.suppressEscapedStrings)
        case '{ ($x: SJConfig).preferTypeHints }            => Some(x.valueOrAbort.preferTypeHints)
  }

  private[scalajack] given ToExpr[TryPolicy] with {
    def apply(x: TryPolicy)(using Quotes): Expr[TryPolicy] =
      x match
        case TryPolicy.AS_NULL         => '{ TryPolicy.AS_NULL }
        case TryPolicy.ERR_MSG_STRING  => '{ TryPolicy.ERR_MSG_STRING }
        case TryPolicy.THROW_EXCEPTION => '{ TryPolicy.THROW_EXCEPTION }
  }

  private[scalajack] given ToExpr[EitherLeftPolicy] with {
    def apply(x: EitherLeftPolicy)(using Quotes): Expr[EitherLeftPolicy] =
      x match
        case EitherLeftPolicy.AS_VALUE        => '{ EitherLeftPolicy.AS_VALUE }
        case EitherLeftPolicy.AS_NULL         => '{ EitherLeftPolicy.AS_NULL }
        case EitherLeftPolicy.ERR_MSG_STRING  => '{ EitherLeftPolicy.ERR_MSG_STRING }
        case EitherLeftPolicy.THROW_EXCEPTION => '{ EitherLeftPolicy.THROW_EXCEPTION }
  }

  private[scalajack] given ToExpr[TypeHintPolicy] with {
    def apply(x: TypeHintPolicy)(using Quotes): Expr[TypeHintPolicy] =
      x match
        case TypeHintPolicy.SIMPLE_CLASSNAME   => '{ TypeHintPolicy.SIMPLE_CLASSNAME }
        case TypeHintPolicy.SCRAMBLE_CLASSNAME => '{ TypeHintPolicy.SCRAMBLE_CLASSNAME }
        case TypeHintPolicy.USE_ANNOTATION     => '{ TypeHintPolicy.USE_ANNOTATION }
  }

  private[scalajack] given FromExpr[TryPolicy] with {
    def unapply(x: Expr[TryPolicy])(using Quotes): Option[TryPolicy] =
      import quotes.reflect.*
      x match
        case '{ TryPolicy.AS_NULL }         => Some(TryPolicy.AS_NULL)
        case '{ TryPolicy.ERR_MSG_STRING }  => Some(TryPolicy.ERR_MSG_STRING)
        case '{ TryPolicy.THROW_EXCEPTION } => Some(TryPolicy.THROW_EXCEPTION)
  }

  private[scalajack] given FromExpr[EitherLeftPolicy] with {
    def unapply(x: Expr[EitherLeftPolicy])(using Quotes): Option[EitherLeftPolicy] =
      import quotes.reflect.*
      x match
        case '{ EitherLeftPolicy.AS_VALUE }        => Some(EitherLeftPolicy.AS_VALUE)
        case '{ EitherLeftPolicy.AS_NULL }         => Some(EitherLeftPolicy.AS_NULL)
        case '{ EitherLeftPolicy.ERR_MSG_STRING }  => Some(EitherLeftPolicy.ERR_MSG_STRING)
        case '{ EitherLeftPolicy.THROW_EXCEPTION } => Some(EitherLeftPolicy.THROW_EXCEPTION)
  }

  private[scalajack] given FromExpr[TypeHintPolicy] with {
    def unapply(x: Expr[TypeHintPolicy])(using Quotes): Option[TypeHintPolicy] =
      import quotes.reflect.*
      x match
        case '{ TypeHintPolicy.SIMPLE_CLASSNAME }   => Some(TypeHintPolicy.SIMPLE_CLASSNAME)
        case '{ TypeHintPolicy.SCRAMBLE_CLASSNAME } => Some(TypeHintPolicy.SCRAMBLE_CLASSNAME)
        case '{ TypeHintPolicy.USE_ANNOTATION }     => Some(TypeHintPolicy.USE_ANNOTATION)
  }

  /*
  Here's how we use Quotes to get default values from a class...def

          // Constructor argument list, preloaded with optional 'None' values and any default values specified
        val preloaded = Expr
          .ofList(r.fields.map { f =>
            val scalaF = f.asInstanceOf[ScalaFieldInfoRef]
            if scalaF.defaultValueAccessorName.isDefined then
              r.refType match
                case '[t] =>
                  val tpe = TypeRepr.of[t].widen
                  val sym = tpe.typeSymbol
                  val companionBody = sym.companionClass.tree.asInstanceOf[ClassDef].body
                  val companion = Ref(sym.companionModule)
                  companionBody
                    .collect {
                      case defaultMethod @ DefDef(name, _, _, _) if name.startsWith("$lessinit$greater$default$" + (f.index + 1)) =>
                        companion.select(defaultMethod.symbol).appliedToTypes(tpe.typeArgs).asExpr
                    }
                    .headOption
                    .getOrElse(Expr(null.asInstanceOf[Boolean]))
            else if scalaF.fieldRef.isInstanceOf[OptionRef[_]] then Expr(None)
            else Expr(null.asInstanceOf[Int])
          })

   */
