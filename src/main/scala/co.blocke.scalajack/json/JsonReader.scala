package co.blocke.scalajack
package json

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.{RTypeRef, TypedName, Clazzes}
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import co.blocke.scala_reflection.RType
import scala.jdk.CollectionConverters.*
import java.util.concurrent.ConcurrentHashMap
import scala.util.{Failure, Success}


case class ParserRead():
    def expectInt(): Either[ParseError,Long] = Right(1L)

case class Blah(msg: String, age: Int, isOk: Boolean)


object JsonReader:

  def classInstantiator[T:Type]( ref: ClassRef[T] )(using Quotes): Expr[Map[String, ?] => T] = 
    import quotes.reflect.*
    val sym = TypeRepr.of[T].classSymbol.get
    '{
        (fieldMap: Map[String, ?]) =>
            ${
                val tree = Apply(Select.unique(New(TypeIdent(sym)), "<init>"),
                    ref.fields.map{f =>
                        f.fieldRef.refType match
                            case '[t] =>
                                '{ fieldMap(${Expr(f.name)}).asInstanceOf[t] }.asTerm
                    }
                )
                tree.asExpr.asExprOf[T]
            }
    }

  def classParseMap[T:Type]( ref: ClassRef[T] )(using Quotes): Expr[JsonParser => Map[String, JsonConfig=>Either[ParseError, ?]]] = 
    '{
        (parser: JsonParser) => 
            val daList = ${
                val fieldList = ref.fields.map(f => f.fieldRef match
                    case t: PrimitiveRef[?] if t.name == Clazzes.CHAR_CLASS =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>
                                for {
                                    strVal <- parser.expectLabel
                                    charVal = strVal.toArray.headOption match
                                        case Some(c) => Right(c)
                                        case None => ParseError(s"Cannot convert value '$strVal' into a Char.")                                    
                                } yield charVal
                            }
                        }
                    case t: PrimitiveRef[?] if t.family == PrimFamily.Stringish =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>parser.expectLabel}
                        }
                    case t: PrimitiveRef[?] if t.name == Clazzes.INT_CLASS =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>
                                for {
                                    longVal <- parser.expectLong(j)
                                    intVal = longVal.toInt
                                } yield intVal
                            }
                        }
                    case t: PrimitiveRef[?] if t.name == Clazzes.SHORT_CLASS =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>
                                for {
                                    longVal <- parser.expectLong(j)
                                    intVal = longVal.toShort
                                } yield intVal
                            }
                        }
                    case t: PrimitiveRef[?] if t.name == Clazzes.BYTE_CLASS =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>
                                for {
                                    longVal <- parser.expectLong(j)
                                    intVal = longVal.toByte
                                } yield intVal
                            }
                        }
                    case t: PrimitiveRef[?] if t.family == PrimFamily.Longish =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>parser.expectLong(j)}
                        }
                    case t: PrimitiveRef[?] if t.name == Clazzes.FLOAT_CLASS =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>
                                for {
                                    longVal <- parser.expectDouble(j)
                                    intVal = longVal.toFloat
                                } yield intVal
                            }
                        }
                    case t: PrimitiveRef[?] if t.family == PrimFamily.Doublish =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>parser.expectDouble(j)}
                        }
                    case t: PrimitiveRef[?] if t.family == PrimFamily.Boolish =>
                        '{
                            ${Expr(f.name)} -> {(j:JsonConfig)=>parser.expectBoolean(j)}
                        }
                )
                Expr.ofList(fieldList)
            }
            daList.asInstanceOf[List[(String, JsonConfig=>Either[ParseError, ?])]].toMap
    }

