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
  val writeMethodDefs: mutable.ArrayBuffer[DefDef] = mutable.ArrayBuffer.empty

  val readMethodSyms: mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  val readMethodDefs: mutable.HashMap[TypedName, DefDef] = mutable.HashMap.empty

  // Used by Reader -- for val defs
  val classFieldMatrixSyms:mutable.HashMap[TypedName, Symbol] = mutable.HashMap.empty
  val classFieldMatrixValDefs: mutable.ArrayBuffer[ValDef] = mutable.ArrayBuffer.empty

  val readerFnMapEntries: mutable.HashMap[TypedName, Expr[JsonSource => Any]] = mutable.HashMap.empty

  val readerMapSym: Symbol = Symbol.newVal(
    Symbol.spliceOwner,
    "readerMap",
    TypeRepr.of[Map[String, JsonSource => Any]],
    Flags.Lazy,
    Symbol.noSymbol
  )