package co.blocke.scalajack
package json

import scala.quoted.*
import scala.collection.mutable
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import writing.JsonOutput
import reading.JsonSource

class CodecBuildContext(using val quotes: Quotes):
  import quotes.reflect.*

  // Tracks which TypedNames (i.e. classes with generic parameters) we've already processed
  val seenBefore: mutable.Map[TypedName, Boolean] = mutable.Map.empty

  // ---------- Writing Support ----------

  // Holds symbols of emitted writer methods for each type (used to avoid regenerating)
  val writeMethodSyms: mutable.HashMap[TypedName, quotes.reflect.Symbol] = mutable.HashMap.empty

  // Holds method definitions (DefDef) for writer functions per TypedName
  val writeMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  // Holds pre-generated writer lambdas: (value, output) => Unit for each TypedName
  val writerFnMapEntries: mutable.HashMap[TypedName, Expr[(Any, JsonOutput) => Unit]] = mutable.HashMap.empty

  // Symbol for the generated `lazy val writerMap`, which maps canonical class names to writer functions
  val writerMapSym: Symbol = Symbol.newVal(
    Symbol.spliceOwner,
    "writerMap",
    TypeRepr.of[Map[String, (Any, JsonOutput) => Unit]],
    Flags.Lazy,
    Symbol.noSymbol
  )

  // ---------- Reading Support ----------

  // Holds symbols of emitted reader methods for each type (used to avoid regenerating)
  val readMethodSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty

  // Holds method definitions (DefDef) for reader functions per TypedName
  val readMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  // Maps a TypedName to a symbol representing the StringMatrix used to resolve field names → field numbers
  val classFieldMatrixSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty

  // Stores the actual ValDefs of those StringMatrix values used in generated deserialization logic
  val classFieldMatrixDefDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty
//  val classFieldMatrixValDefs: mutable.HashMap[TypedName, ValDef] = mutable.HashMap.empty

  // Maps TypedName → ReaderEntry (a wrapper for a generated reader function)
  // Used for resolving recursive/self-referencing types at code-gen time
  val readerFnMap: mutable.HashMap[TypedName, reading.ReaderEntry] = mutable.HashMap.empty

  // Symbol for the generated `lazy val readerMap`, which maps canonical class names to reader functions
  val readerMapSym: Symbol = Symbol.newVal(
    Symbol.spliceOwner,
    "readerMap",
    TypeRepr.of[Map[String, JsonSource => Any]],
    Flags.Lazy,
    Symbol.noSymbol
  )

  // ---------- Special Type Handling ----------

  // Whether any SelfRefRef was seen (used to decide if readerMap should be emitted)
  var seenSelfRef: Boolean = false

  // Whether a field of type `Any` was seen (used to decide if readAny() should be emitted)
  var seenAnyRef: Boolean = false

  // Symbol for the special method `readAny(in: JsonSource): Any`
  // This gets generated if any `Any`-typed fields appear in the model
  val readAnySym: Symbol = Symbol.newMethod(
    Symbol.spliceOwner,
    "readAny",
    MethodType(List("in"))(_ => List(TypeRepr.of[JsonSource]), _ => TypeRepr.of[Any])
  )

  // The actual implementation of `readAny`, which handles all valid JSON types dynamically
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
