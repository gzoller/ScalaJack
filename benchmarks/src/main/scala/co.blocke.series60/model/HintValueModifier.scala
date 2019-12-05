
package co.blocke.series60
package model

import scala.reflect.runtime.universe.Type
import util.BijectiveFunction
import util.BijectiveFunctionHelpers._

trait HintValueModifier extends BijectiveFunction[String, Type]

/**
 * Convenience modifier that transforms a hint value string into a fully-qualified class name (and the reverse)
 * using passed-in transformation functions.  The appropriate Bijective is created under the covers.
 */
case class ClassNameHintModifier(hintToClassname: (String) => String, classNameToHint: (String) => String) extends HintValueModifier {
  def apply(rawHint: String) = fullNameToType.apply(hintToClassname(rawHint)) // May explode
  def unapply(hintFieldType: Type) = classNameToHint(fullNameToType.unapply(hintFieldType)) // May explode
}

/**
 * Convenience modifier that transforms a map of string hint values to their respective types.
 * Note there is a necessary assumption that the mapping is 1-to-1.  If not you'll need to create the
 * Bijective function yourself with whatever logic you need, and not use this class.
 */
case class StringMatchHintModifier(hintToType: Map[String, Type]) extends HintValueModifier {
  val typeToHint = hintToType.map(_.swap)
  def apply(rawHint: String) = hintToType(rawHint) // May explode
  def unapply(hintFieldType: Type) = typeToHint(hintFieldType) // May explode
}