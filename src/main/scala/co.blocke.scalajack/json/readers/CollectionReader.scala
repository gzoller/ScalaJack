package co.blocke.scalajack
package json
package readers

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import scala.collection.Factory
import co.blocke.scala_reflection.RType
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class ColletionReader(next: ReaderModule, root: ReaderModule) extends ReaderModule:

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import quotes.reflect.*

    ref match
      case t: SeqRef[T] =>
        if isMapKey then throw new JsonError("Seq types cannot be map keys.")
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = root.readerFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn).map(_.to(${ Expr.summon[Factory[e, T]].get })) // Convert List to whatever the target type should be
                }

      case t: MapRef[T] =>
        if isMapKey then throw new JsonError("Map types cannot be map keys.")
        t.refType match
          case '[m] =>
            t.elementRef.refType match
              case '[k] =>
                t.elementRef2.refType match
                  case '[v] =>
                    val keyFn = root.readerFn[k](t.elementRef.asInstanceOf[RTypeRef[k]], true).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, k]]]
                    val valFn = root.readerFn[v](t.elementRef2.asInstanceOf[RTypeRef[v]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, v]]]
                    '{ (j: JsonConfig, p: JsonParser) =>
                      p.expectObject[k, v](j, $keyFn, $valFn).map(_.to(${ Expr.summon[Factory[(k, v), T]].get })) // Convert List to whatever the target type should be
                    }

      case t: ArrayRef[T] =>
        if isMapKey then throw new JsonError("Arrays cannot be map keys.")
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = root.readerFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                '{ (j: JsonConfig, p: JsonParser) =>
                  p.expectList[e](j, $subFn)
                    .map(_ match
                      case v if v == null => null.asInstanceOf[T]
                      case v              => v.to(${ Expr.summon[Factory[e, T]].get })
                    ) // Convert List to whatever the target type should be
                }

      case t: TupleRef[T] =>
        if isMapKey then throw new JsonError("Tuple types cannot be map keys.")
        t.refType match
          case '[s] =>
            val tupleFns = Expr.ofList(
              t.tupleRefs.map(tr =>
                tr.refType match
                  case '[e] =>
                    root.readerFn[e](tr.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
              )
            )
            val instantiator = JsonReaderUtil.tupleInstantiator[T](t)
            '{ (j: JsonConfig, p: JsonParser) =>
              p.expectTuple(j, $tupleFns)
                .flatMap(results =>
                  scala.util.Try($instantiator(results)) match // instantiate the tuple here!!!
                    case Success(v) => Right(v)
                    case Failure(e) => Left(JsonParseError(p.showError(s"Unable to instantiate tuple at position [${p.getPos - 1}] with message ${e.getMessage}")))
                )
            }

      // Java
      case t: JavaCollectionRef[T] =>
        if isMapKey then throw new JsonError("Java collections cannot be map keys.")
        t.refType match
          case '[s] =>
            t.elementRef.refType match
              case '[e] =>
                val subFn = root.readerFn[e](t.elementRef.asInstanceOf[RTypeRef[e]]).asInstanceOf[Expr[(JsonConfig, JsonParser) => Either[ParseError, e]]]
                val className = Expr(t.name)
                '{ (j: JsonConfig, p: JsonParser) =>
                  val cname = $className
                  p.expectList[e](j, $subFn)
                    .flatMap(result =>
                      scala.util.Try(Class.forName(cname).getDeclaredConstructor(Class.forName("java.util.Collection")).newInstance(result.asJava)) match
                        case Success(ok) => Right(ok.asInstanceOf[T])
                        case Failure(e)  => Left(JsonParseError(p.showError(s"Could not instantiate a $cname, with error: " + e)))
                    )
                }

      case t =>
        next.readerFn[T](t)
