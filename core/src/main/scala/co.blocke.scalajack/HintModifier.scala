package co.blocke.scalajack

import co.blocke.scalajack.BijectiveFunctions._

trait HintModifier extends BijectiveFunction[String, Type]

/**
 * Convenience modifier that transforms a hint value string into a fully-qualified class name (and the reverse)
 * using passed-in transformation functions.  The appropriate Bijective is created under the covers.
 */
case class ClassNameHintModifier(hintToClassname: (String) => String, classNameToHint: (String) => String) extends HintModifier {
  def apply(rawHint: String) = fullNameToType.apply(hintToClassname(rawHint))
  def unapply(hintFieldType: Type) = classNameToHint(fullNameToType.unapply(hintFieldType))
}

/**
 * Convenience modifier that transforms a map of string hint values to their respective types.
 * Note there is a necessary assumption that the mapping is 1-to-1.  If not you'll need to create the
 * Bijective function yourself with whatever logic you need, and not use this class.
 */
case class StringMatchHintModifier(hintToType: Map[String, Type]) extends HintModifier {
  val typeToHint = hintToType.map(_.swap)
  def apply(rawHint: String) =
    hintToType.getOrElse(rawHint, throw new IllegalStateException("No Type mapping given for hint " + rawHint))
  def unapply(hintFieldType: Type) =
    typeToHint.getOrElse(hintFieldType, throw new IllegalStateException("No hint value mapping given for Type " + hintFieldType.typeSymbol.fullName))
}