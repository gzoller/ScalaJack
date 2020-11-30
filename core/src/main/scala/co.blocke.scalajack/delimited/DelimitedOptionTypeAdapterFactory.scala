package co.blocke.scalajack
package delimited

import typeadapter.{OptionTypeAdapter, JavaOptionalTypeAdapter}
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.info.{OptionInfo, ScalaOptionInfo, JavaOptionalInfo, TypeSymbolInfo}

import model._

/**
 * Options are handled a little differently for Delimited.  They should result in an empty field.
 * Empty fields area always read in as None, so no null fields are possible for Delimited options.
 */
object DelimitedOptionTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean =
    concrete match {
      case _: OptionInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val optiBase = concrete.asInstanceOf[OptionInfo]
    val wrapped = optiBase.optionParamType match {
      case c: TypeSymbolInfo => throw new ScalaJackError(s"Unexpected non-concrete type in option: ${c.name}")
      case c => taCache.typeAdapterOf(c)
    }
    concrete match {
      case opti: ScalaOptionInfo   => OptionTypeAdapter(concrete, wrapped, true)  // Note nullAsNone = true here!
      case jopti: JavaOptionalInfo => JavaOptionalTypeAdapter(concrete, wrapped, true)
    }
