package co.blocke.scalajack
package json

import co.blocke.scala_reflection.TypedName
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*

class JsonConfig private[scalajack] (
    val noneAsNull: Boolean,
    // val forbidNullsInInput: Boolean = false,
    val tryFailureHandling: TryPolicy,
    val eitherLeftHandling: EitherLeftPolicy,
    // val undefinedFieldHandling: UndefinedValueOption = UndefinedValueOption.THROW_EXCEPTION,
    // val permissivePrimitives: Boolean = false,
    val writeNonConstructorFields: Boolean,
    // --------------------------
    val typeHintLabel: String,
    val typeHintPolicy: TypeHintPolicy,
    // --------------------------
    val enumsAsIds: Option[List[String]], // None=no enums as ids, Some(Nil)=all enums as ids, Some(List(...))=specified classes enums as ids
    val escapedStrings: Boolean
):
  def withNoneAsNull(): JsonConfig = copy(noneAsNull = true)
  def withTryFailureHandling(tryPolicy: TryPolicy): JsonConfig = copy(tryFailureHandling = tryPolicy)
  def withEitherLeftHandling(eitherPolicy: EitherLeftPolicy): JsonConfig = copy(eitherLeftHandling = eitherPolicy)
  def withWriteNonConstructorFields(nonConstFlds: Boolean): JsonConfig = copy(writeNonConstructorFields = nonConstFlds)
  def withTypeHintLabel(label: String): JsonConfig = copy(typeHintLabel = label)
  def withTypeHintPolicy(hintPolicy: TypeHintPolicy): JsonConfig = copy(typeHintPolicy = hintPolicy)
  def withEnumsAsIds(asIds: Option[List[String]]): JsonConfig = copy(enumsAsIds = asIds)
  def withEscapedStrings(): JsonConfig = copy(escapedStrings = false)

  private[this] def copy(
      noneAsNull: Boolean = noneAsNull,
      tryFailureHandling: TryPolicy = tryFailureHandling,
      eitherLeftHandling: EitherLeftPolicy = eitherLeftHandling,
      writeNonConstructorFields: Boolean = writeNonConstructorFields,
      typeHintLabel: String = typeHintLabel,
      typeHintPolicy: TypeHintPolicy = typeHintPolicy,
      enumsAsIds: Option[List[String]] = enumsAsIds,
      escapedStrings: Boolean = escapedStrings
  ): JsonConfig = new JsonConfig(
    noneAsNull,
    tryFailureHandling,
    eitherLeftHandling,
    writeNonConstructorFields,
    typeHintLabel,
    typeHintPolicy,
    enumsAsIds,
    escapedStrings
  )

enum TryPolicy:
  case AS_NULL, NO_WRITE, ERR_MSG_STRING, THROW_EXCEPTION

enum EitherLeftPolicy:
  case AS_VALUE, AS_NULL, NO_WRITE, ERR_MSG_STRING, THROW_EXCEPTION

// enum UndefinedValueOption:
//   case AS_NULL, AS_SYMBOL, THROW_EXCEPTION

enum TypeHintPolicy:
  case SIMPLE_CLASSNAME, SCRAMBLE_CLASSNAME, USE_ANNOTATION

object JsonConfig
    extends JsonConfig(
      noneAsNull = false,
      tryFailureHandling = TryPolicy.NO_WRITE,
      eitherLeftHandling = EitherLeftPolicy.NO_WRITE,
      writeNonConstructorFields = true,
      typeHintLabel = "_hint",
      typeHintPolicy = TypeHintPolicy.SIMPLE_CLASSNAME,
      enumsAsIds = None,
      escapedStrings = false
    ):
  import scala.quoted.FromExpr.*

  private[scalajack] given ToExpr[JsonConfig] with {
    def apply(x: JsonConfig)(using Quotes): Expr[JsonConfig] =
      '{
        val jc = JsonConfig
          .withTryFailureHandling(${ Expr(x.tryFailureHandling) })
          .withEitherLeftHandling(${ Expr(x.eitherLeftHandling) })
          .withWriteNonConstructorFields(${ Expr(x.writeNonConstructorFields) })
          .withTypeHintLabel(${ Expr(x.typeHintLabel) })
          .withTypeHintPolicy(${ Expr(x.typeHintPolicy) })
          .withEnumsAsIds(${ Expr(x.enumsAsIds) })
        val jc2 = ${
          if x.noneAsNull then '{ jc.withNoneAsNull() }
          else '{ jc }
        }
        val jc3 = ${
          if !x.escapedStrings then '{ jc2.withEscapedStrings() }
          else '{ jc2 }
        }
        jc3
      }
  }

  private[scalajack] given FromExpr[JsonConfig] with {

    def extract[X: FromExpr](name: String, x: Expr[X])(using Quotes): X =
      import quotes.reflect.*
      summon[FromExpr[X]].unapply(x).getOrElse(throw JsonConfigError(s"Can't parse $name: ${x.show}, tree: ${x.asTerm}"))

    def unapply(x: Expr[JsonConfig])(using Quotes): Option[JsonConfig] =
      import quotes.reflect.*

      x match
        case '{
              JsonConfig(
                $noneAsNullE,
                // $forbitNullsInInputE,
                $tryFailureHandlerE,
                $eitherLeftHandlerE,
                // $undefinedFieldHandlingE,
                // $permissivePrimitivesE,
                $writeNonConstructorFieldsE,
                $typeHintLabelE,
                $typeHintPolicyE,
                $enumsAsIdsE,
                $escapedStringsE
              )
            } =>
          try
            Some(
              JsonConfig(
                extract("noneAsNull", noneAsNullE),
                // extract("forbitNullsInInput", forbitNullsInInputE),
                extract("tryFailureHandler", tryFailureHandlerE),
                extract("eitherLeftHandler", eitherLeftHandlerE),
                // extract("undefinedFieldHandling", undefinedFieldHandlingE),
                // extract("permissivePrimitives", permissivePrimitivesE),
                extract("writeNonConstructorFields", writeNonConstructorFieldsE),
                // extract2[String]("typeHintLabel", x)
                extract("typeHintLabel", typeHintLabelE),
                extract("typeHintPolicy", typeHintPolicyE),
                extract("enumsAsIds", enumsAsIdsE),
                extract("escapedStrings", escapedStringsE)
              )
            )
          catch {
            case x =>
              println("ERROR: " + x.getMessage)
              None
          }
        case '{ JsonConfig }                                         => Some(JsonConfig)
        case '{ ($x: JsonConfig).withNoneAsNull() }                  => Some(x.valueOrAbort.withNoneAsNull())
        case '{ ($x: JsonConfig).withTryFailureHandling($v) }        => Some(x.valueOrAbort.withTryFailureHandling(v.valueOrAbort))
        case '{ ($x: JsonConfig).withEitherLeftHandling($v) }        => Some(x.valueOrAbort.withEitherLeftHandling(v.valueOrAbort))
        case '{ ($x: JsonConfig).withWriteNonConstructorFields($v) } => Some(x.valueOrAbort.withWriteNonConstructorFields(v.valueOrAbort))
        case '{ ($x: JsonConfig).withTypeHintLabel($v) }             => Some(x.valueOrAbort.withTypeHintLabel(v.valueOrAbort))
        case '{ ($x: JsonConfig).withTypeHintPolicy($v) }            => Some(x.valueOrAbort.withTypeHintPolicy(v.valueOrAbort))
        case '{ ($x: JsonConfig).withEnumsAsIds($v) }                => Some(x.valueOrAbort.withEnumsAsIds(v.valueOrAbort))
        case '{ ($x: JsonConfig).withEscapedStrings() }              => Some(x.valueOrAbort.withEscapedStrings())
  }

  private[scalajack] given ToExpr[TryPolicy] with {
    def apply(x: TryPolicy)(using Quotes): Expr[TryPolicy] =
      x match
        case TryPolicy.AS_NULL         => '{ TryPolicy.AS_NULL }
        case TryPolicy.ERR_MSG_STRING  => '{ TryPolicy.ERR_MSG_STRING }
        case TryPolicy.NO_WRITE        => '{ TryPolicy.NO_WRITE }
        case TryPolicy.THROW_EXCEPTION => '{ TryPolicy.THROW_EXCEPTION }
  }

  private[scalajack] given ToExpr[EitherLeftPolicy] with {
    def apply(x: EitherLeftPolicy)(using Quotes): Expr[EitherLeftPolicy] =
      x match
        case EitherLeftPolicy.AS_VALUE        => '{ EitherLeftPolicy.AS_VALUE }
        case EitherLeftPolicy.AS_NULL         => '{ EitherLeftPolicy.AS_NULL }
        case EitherLeftPolicy.ERR_MSG_STRING  => '{ EitherLeftPolicy.ERR_MSG_STRING }
        case EitherLeftPolicy.NO_WRITE        => '{ EitherLeftPolicy.NO_WRITE }
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
        case '{ TryPolicy.NO_WRITE }        => Some(TryPolicy.NO_WRITE)
        case '{ TryPolicy.ERR_MSG_STRING }  => Some(TryPolicy.ERR_MSG_STRING)
        case '{ TryPolicy.THROW_EXCEPTION } => Some(TryPolicy.THROW_EXCEPTION)
  }

  private[scalajack] given FromExpr[EitherLeftPolicy] with {
    def unapply(x: Expr[EitherLeftPolicy])(using Quotes): Option[EitherLeftPolicy] =
      import quotes.reflect.*
      x match
        case '{ EitherLeftPolicy.AS_VALUE }        => Some(EitherLeftPolicy.AS_VALUE)
        case '{ EitherLeftPolicy.AS_NULL }         => Some(EitherLeftPolicy.AS_NULL)
        case '{ EitherLeftPolicy.NO_WRITE }        => Some(EitherLeftPolicy.NO_WRITE)
        case '{ EitherLeftPolicy.ERR_MSG_STRING }  => Some(EitherLeftPolicy.ERR_MSG_STRING)
        case '{ EitherLeftPolicy.THROW_EXCEPTION } => Some(EitherLeftPolicy.THROW_EXCEPTION)
  }

  // private[scalajack] given FromExpr[UndefinedValueOption] with {
  //   def unapply(x: Expr[UndefinedValueOption])(using Quotes): Option[UndefinedValueOption] =
  //     import quotes.reflect.*
  //     x match
  //       case '{ UndefinedValueOption.AS_NULL }         => Some(UndefinedValueOption.AS_NULL)
  //       case '{ UndefinedValueOption.AS_SYMBOL }       => Some(UndefinedValueOption.AS_SYMBOL)
  //       case '{ UndefinedValueOption.THROW_EXCEPTION } => Some(UndefinedValueOption.THROW_EXCEPTION)
  // }

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
