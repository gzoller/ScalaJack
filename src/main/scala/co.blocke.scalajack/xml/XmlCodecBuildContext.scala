package co.blocke.scalajack
package xml

import internal.CodecBuildContext
import scala.quoted.*
import scala.collection.mutable
import co.blocke.scala_reflection.TypedName

class XmlCodecBuildContext(using override val quotes: Quotes) extends CodecBuildContext:
  import quotes.reflect.*

  // Symbol for the special method `readAny(in: JsonSource): Any`
  // This gets generated if any `Any`-typed fields appear in the model
  val readAnySym: Symbol = null.asInstanceOf[Symbol]

  // The actual implementation of `readAny`, which handles all valid JSON types dynamically
  val readAnyDef: DefDef = null.asInstanceOf[DefDef]