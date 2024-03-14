package co.blocke.scalajack
package json
package schema

import java.net.URL
import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import org.apache.commons.text.StringEscapeUtils

// Macro...
import scala.quoted.*

object JsonSchema:

    inline def of[T]: Schema = ${ ofImpl[T]() }
    def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[Schema] =
        import quotes.reflect.*
        val classRef = ReflectOnType[T](quotes)(TypeRepr.of[T], true)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
        genSchema(quotes)( classRef, None)

    private def genSchema[T](quotes: Quotes)(rt: RTypeRef[T], context: Option[ScalaFieldInfoRef] = None, defaultValue: Option[quotes.reflect.Term] = None): Expr[Schema] = 
        import quotes.reflect.*
        implicit val q: Quotes = quotes

        def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using Quotes): Expr[Option[T]] =
            if xs.isEmpty then Expr(None) else '{ Some(${xs.get}) }

        def typeArgs(tpe: TypeRepr): List[TypeRepr] = tpe match
            case AppliedType(_, typeArgs) => typeArgs.map(_.dealias)
            case _                        => Nil

        rt match
            case t: AliasRef[_] => 
                t.unwrappedType.refType match 
                    case '[e] =>
                        genSchema[e](quotes)(t.unwrappedType.asInstanceOf[RTypeRef[e]], context, defaultValue)
            case t: ArrayRef[_] => 
                t.refType match 
                    case '[u] => 
                        t.elementRef.refType match
                            case '[e] =>
                                '{
                                    ArraySchema( 
                                        ${ genSchema[e](quotes)(t.elementRef.asInstanceOf[RTypeRef[e]], context, None) },
                                        ${ Expr(context.flatMap(_.annotations.get("minItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("maxItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("uniqueItems")).flatMap(_.get("value")).map(_.toBoolean)) },
                                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                                        ${
                                            if defaultValue.isDefined then 
                                                val codec = JsonCodecMaker.generateCodecFor[u](t.refType.asInstanceOf[RTypeRef[u]], json.JsonConfig)
                                                '{ 
                                                    val out = new writing.JsonOutput()
                                                    $codec.encodeValue(${defaultValue.get.asExprOf[u]}, out)
                                                    Some(out.toString.asInstanceOf[RawJson])
                                                }
                                            else 
                                                Expr(None)
                                        }
                                    )
                                }
            case t: EnumRef[_] => '{ EnumSchema( ${Expr(t.values)} ) }
            case t: OptionRef[_] =>
                // Go ahead and gen schema for body of Option.  Higher level (ie class) is resposible for tracking if a
                // value is required or not...
                t.optionParamType.refType match
                    case '[e] =>
                        defaultValue.map{ dv => 
                            val dve = dv.asExprOf[Option[e]]
                            '{
                                $dve match
                                    case Some(a) => ${ genSchema[e](quotes)(t.optionParamType.asInstanceOf[RTypeRef[e]], context, Some('{a}.asTerm)) }
                                    case None => ${ genSchema[e](quotes)(t.optionParamType.asInstanceOf[RTypeRef[e]], context, None) }
                            }
                        }.getOrElse( genSchema[e](quotes)(t.optionParamType.asInstanceOf[RTypeRef[e]], context, None) )
            case t: TryRef[_] =>
                // Go ahead and gen schema for body of Try.  Higher level (ie class) is resposible for tracking if a
                // value is required or not...
                t.tryRef.refType match
                    case '[e] =>
                        genSchema[e](quotes)(t.tryRef.asInstanceOf[RTypeRef[e]], context, None)     
            case t: SeqRef[_] =>
                t.refType match 
                    case '[u] => 
                        t.elementRef.refType match
                            case '[e] =>
                                '{
                                    ArraySchema( 
                                        ${ genSchema[e](quotes)(t.elementRef.asInstanceOf[RTypeRef[e]], context, None) },
                                        ${ Expr(context.flatMap(_.annotations.get("minItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("maxItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("uniqueItems")).flatMap(_.get("value")).map(_.toBoolean)) },
                                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                                        ${
                                            if defaultValue.isDefined then 
                                                val codec = JsonCodecMaker.generateCodecFor[u](t.asInstanceOf[RTypeRef[u]], json.JsonConfig)
                                                '{ 
                                                    val out = new writing.JsonOutput()
                                                    $codec.encodeValue(${defaultValue.get.asExprOf[u]}, out)
                                                    Some(out.result.asInstanceOf[RawJson])
                                                }
                                            else 
                                                Expr(None)
                                        }
                                    )
                                }
            case t: SetRef[_] =>
                t.refType match 
                    case '[u] => 
                        t.elementRef.refType match
                            case '[e] =>
                                '{
                                    ArraySchema( 
                                        ${ genSchema[e](quotes)(t.elementRef.asInstanceOf[RTypeRef[e]], context, None) },
                                        ${ Expr(context.flatMap(_.annotations.get("minItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("maxItems")).flatMap(_.get("value")).map(_.toInt)) },
                                        ${ Expr(context.flatMap(_.annotations.get("uniqueItems")).flatMap(_.get("value")).map(_.toBoolean)) },
                                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                                        ${
                                            if defaultValue.isDefined then 
                                                val codec = JsonCodecMaker.generateCodecFor[u](t.asInstanceOf[RTypeRef[u]], json.JsonConfig)
                                                '{ 
                                                    val out = new writing.JsonOutput()
                                                    $codec.encodeValue(${defaultValue.get.asExprOf[u]}, out)
                                                    Some(out.result.asInstanceOf[RawJson])
                                                }
                                            else 
                                                Expr(None)
                                        }
                                    )
                                }
            case t: TupleRef[_] => 
                t.refType match 
                    case '[u] => 
                        '{
                            TupleSchema(
                                ${
                                    Expr.ofList(t.tupleRefs.map{ tr =>
                                        tr.refType match
                                            case '[w] =>
                                                genSchema[w](quotes)(tr.asInstanceOf[RTypeRef[w]], context, None)  
                                    })
                                },
                                ${ Expr(context.flatMap(_.annotations.get("items")).flatMap(_.get("value")).map(_.toBoolean)) },
                                ${ Expr(context.flatMap(_.annotations.get("minItems")).flatMap(_.get("value")).map(_.toInt)) },
                                ${ Expr(context.flatMap(_.annotations.get("maxItems")).flatMap(_.get("value")).map(_.toInt)) },
                                ${ Expr(context.flatMap(_.annotations.get("uniqueItems")).flatMap(_.get("value")).map(_.toBoolean)) },
                                ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                                ${
                                    if defaultValue.isDefined then 
                                        val codec = JsonCodecMaker.generateCodecFor[u](t.asInstanceOf[RTypeRef[u]], json.JsonConfig)
                                        '{ 
                                            val out = new writing.JsonOutput()
                                            $codec.encodeValue(${defaultValue.get.asExprOf[u]}, out)
                                            Some(out.result.asInstanceOf[RawJson])
                                        }
                                    else 
                                        Expr(None)
                                }
                            )
                        }
            case t: BooleanRef => 
                '{
                    BooleanSchema(
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ${v.asExprOf[Boolean]}.toString.asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: DoubleRef => 
                '{
                    NumberSchema(
                        ${ Expr(context.flatMap(_.annotations.get("minimum")).flatMap(_.get("value")).map(_.toDouble)) },
                        ${ Expr(context.flatMap(_.annotations.get("maximum")).flatMap(_.get("value")).map(_.toDouble)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMinimum")).flatMap(_.get("value")).map(_.toDouble)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMaximum")).flatMap(_.get("value")).map(_.toDouble)) },
                        ${ Expr(context.flatMap(_.annotations.get("multipleOf")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ${v.asExprOf[Double]}.toString.asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: IntRef => 
                '{
                    IntegerSchema(
                        ${ Expr(context.flatMap(_.annotations.get("minimum")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("maximum")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMinimum")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMaximum")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("multipleOf")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ${v.asExprOf[Int]}.toString.asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: LongRef => 
                '{
                    IntegerSchema(
                        ${ Expr(context.flatMap(_.annotations.get("minimum")).flatMap(_.get("value")).map(_.toLong)) },
                        ${ Expr(context.flatMap(_.annotations.get("maximum")).flatMap(_.get("value")).map(_.toLong)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMinimum")).flatMap(_.get("value")).map(_.toLong)) },
                        ${ Expr(context.flatMap(_.annotations.get("exclusiveMaximum")).flatMap(_.get("value")).map(_.toLong)) },
                        ${ Expr(context.flatMap(_.annotations.get("multipleOf")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ${v.asExprOf[Long]}.toString.asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: StringRef => 
                '{
                    StringSchema(
                        ${ Expr(context.flatMap(_.annotations.get("minLength")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("maxLength")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("pattern")).flatMap(_.get("value")).map(v => StringEscapeUtils.escapeJson(v))) },
                        ${ Expr(context.flatMap(_.annotations.get("format")).flatMap(_.get("value"))) }.map(v => StringFormat.valueOf(v)),
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ("\"" + StringEscapeUtils.escapeJson(${v.asExprOf[String]}) + "\"").asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: ZonedDateTimeRef => 
                '{
                    StringSchema(
                        ${ Expr(context.flatMap(_.annotations.get("minLength")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("maxLength")).flatMap(_.get("value")).map(_.toInt)) },
                        ${ Expr(context.flatMap(_.annotations.get("pattern")).flatMap(_.get("value")).map(v => StringEscapeUtils.escapeJson(v))) },
                        Some(StringFormat.`date-time`),
                        ${ Expr(context.flatMap(_.annotations.get("description")).flatMap(_.get("value"))) },
                        ${
                            ofOption(defaultValue.map(v => '{ ${v.asExprOf[java.time.ZonedDateTime]}.toString.asInstanceOf[RawJson] }))
                        }
                    )
                }
            case t: ScalaClassRef[_] =>
                t.refType match 
                    case '[u] => 
                        val requiredFields = Expr(t.fields.collect{ 
                            case f: FieldInfoRef if !f.fieldRef.isInstanceOf[OptionRef[_]] => f.name
                        })
                        '{
                            ObjectSchema(
                                ${
                                    Expr.ofList(t.fields.map(f =>
                                        f.fieldRef.refType match
                                            case '[b] =>
                                                // Get default value if any
                                                val tpe = TypeRepr.of[u]
                                                val classCompanion = tpe.typeSymbol.companionClass
                                                val companionModule = tpe.typeSymbol.companionModule
                                                val dvMembers = classCompanion.methodMember("$lessinit$greater$default$" + (f.index + 1))
                                                val fieldDefault = if dvMembers.isEmpty then 
                                                    None
                                                else 
                                                    val methodSymbol = dvMembers.head
                                                    val dvSelectNoTArgs = Ref(companionModule).select(methodSymbol)
                                                    val dvSelect = methodSymbol.paramSymss match
                                                        case Nil => dvSelectNoTArgs
                                                        case List(params) if (params.exists(_.isTypeParam)) =>
                                                            typeArgs(tpe) match
                                                            case Nil      => ??? // throw JsonParseError("Expected an applied type", ???)
                                                            case typeArgs => TypeApply(dvSelectNoTArgs, typeArgs.map(Inferred(_)))
                                                        case _ => ??? // fail(s"Default method for ${symbol.name} of class ${tpe.show} have a complex " +
                                                    Some(dvSelect)
                                                Expr.ofTuple((Expr(f.name), genSchema[b](quotes)(f.fieldRef.asInstanceOf[RTypeRef[b]], Some(f.asInstanceOf[ScalaFieldInfoRef]), fieldDefault)))
                                        )
                                    )
                                }.toMap,
                                $requiredFields,
                                ${ Expr(t.annotations.get("co.blocke.scalajack.schema.additionalProperties").flatMap(_.get("value")).map(_.toBoolean).getOrElse(false)) },
                                new URL("http://jsons-schema.org/draft-04/schema#"),
                                ${ Expr(t.annotations.get("co.blocke.scalajack.schema.id").flatMap(_.get("value"))) },
                                ${ Expr(t.annotations.get("co.blocke.scalajack.schema.title").flatMap(_.get("value"))) },
                                ${ Expr(t.annotations.get("co.blocke.scalajack.schema.description").flatMap(_.get("value"))) },
                                ${
                                    if defaultValue.isDefined then 
                                        val codec = JsonCodecMaker.generateCodecFor[u](t.asInstanceOf[RTypeRef[u]], json.JsonConfig.withSuppressTypeHints())
                                        '{ 
                                            val out = new writing.JsonOutput()
                                            $codec.encodeValue(${defaultValue.get.asExprOf[u]}, out)
                                            Some(out.result.asInstanceOf[RawJson])
                                        }
                                    else 
                                        Expr(None)
                                }
                            )
                        }
            case _ => throw new Exception(s"Unsupported type ${rt.name} for JSON schema generation")
