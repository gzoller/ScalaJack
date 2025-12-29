package co.blocke.scalajack
package shared

import scala.quoted.*
import scala.collection.mutable
import co.blocke.scala_reflection.TypedName

trait CodecBuildContext:

  /** Stable path; safe to use as a type prefix: quotes.reflect.Symbol, DefDef, etc */
  val quotes: Quotes

  /** If you want `using Quotes` to work downstream */
  given Quotes = quotes

  import quotes.reflect.*

  // Tracks which TypedNames (i.e. classes with generic parameters) we've already processed
  val seenBefore: mutable.Map[TypedName, Boolean] = mutable.Map.empty

  // ---------- Writing Support ----------
  // Holds symbols of emitted writer methods for each type (used to avoid regenerating)
  val writeMethodSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  // Holds method definitions (DefDef) for writer functions per TypedName
  val writeMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  // ---------- Reading Support ----------
  // Holds symbols of emitted reader methods for each type (used to avoid regenerating)
  val readMethodSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  // Holds method definitions (DefDef) for reader functions per TypedName
  val readMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  // Whether a field of type `Any` was seen (used to decide if readAny() should be emitted)
  var seenAnyRef: Boolean = false

  // Symbol for the special method `readAny(in: JsonSource): Any`
  // This gets generated if any `Any`-typed fields appear in the model
  val readAnySym: Symbol

  // The actual implementation of `readAny`, which handles all valid JSON types dynamically
  val readAnyDef: DefDef
