package co.blocke.scalajack
package json

import scala.quoted.*
import scala.collection.mutable
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import writing.JsonOutput
import reading.JsonSource

class CodecBuildContext(using val quotes: Quotes):
  import quotes.reflect.*

  val seenBefore: mutable.Map[TypedName, Boolean] = mutable.Map.empty

  val writeMethodSyms: mutable.HashMap[TypedName, quotes.reflect.Symbol] = mutable.HashMap.empty
  val writeMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  val readMethodSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  val readMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  val writerFnMapEntries: mutable.HashMap[TypedName, Expr[(Any, JsonOutput) => Unit]] = mutable.HashMap.empty

  val writerMapSym: Symbol = Symbol.newVal(
    Symbol.spliceOwner,
    "writerMap",
    TypeRepr.of[Map[String, (Any, JsonOutput) => Unit]],
    Flags.Lazy,
    Symbol.noSymbol
  )

  // Used by Reader -- for val defs
  val classFieldMatrixSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  val classFieldMatrixValDefs: mutable.ArrayBuffer[ValDef] = mutable.ArrayBuffer.empty

  val readerFnMapEntries: mutable.HashMap[TypedName, Expr[JsonSource => Any]] = mutable.HashMap.empty

  val readerMapSym: Symbol = Symbol.newVal(
    Symbol.spliceOwner,
    "readerMap",
    TypeRepr.of[Map[String, JsonSource => Any]],
    Flags.Lazy,
    Symbol.noSymbol
  )

  var seenSelfRef: Boolean = false // set to true to generate map entries for readers/writers, otherwise they aren't emitted

  var seenAnyRef: Boolean = false

  val readAnySym: Symbol = Symbol.newMethod(
    Symbol.spliceOwner,
    "readAny",
    MethodType(List("in"))(_ => List(TypeRepr.of[JsonSource]), _ => TypeRepr.of[Any])
  )

  val readAnyDef: DefDef = DefDef(
    readAnySym,
    {
      case List(List(inParam)) =>
        given Quotes = quotes
        import quotes.reflect.*
        val in = Ref(inParam.symbol).asExprOf[JsonSource]

        Some(
          '{
            if $in.expectNull() then null
            else
              $in.readToken() match
                case '[' =>
                  $in.backspace()
                  val buf = $in.expectArray(() => ${ Ref(readAnySym).appliedTo(Ref(inParam.symbol)).asExprOf[Any] })
                  buf.toList
                case '{' =>
                  $in.parseMap[String, Any](
                    () => $in.expectString(),
                    () => ${ Ref(readAnySym).appliedTo(Ref(inParam.symbol)).asExprOf[Any] },
                    Map.empty[String, Any],
                    true
                  )
                case 't' | 'f' =>
                  $in.backspace()
                  $in.expectBoolean()
                case n if n == '-' || n == '+' || n == '.' || (n >= '0' && n <= '9') =>
                  $in.backspace()
                  $in.expectNumberOrNull() match
                    case null => null
                    case s =>
                      scala.math.BigDecimal(s) match {
                        case i if i.isValidInt      => i.toIntExact
                        case i if i.isValidLong     => i.toLongExact
                        case d if d.isDecimalDouble => d.toDouble
                        case d if d.ulp == 1        => d.toBigInt
                        case d                      => d
                      }
                case '"' =>
                  $in.backspace()
                  $in.expectString()
                case _ =>
                  throw new JsonParseError("Illegal JSON char while parsing Any value", $in)
          }.asTerm.changeOwner(readAnySym)
        )
      case other =>
        throw new JsonTypeError(s"Unexpected method parameters: $other")
    }
  )
