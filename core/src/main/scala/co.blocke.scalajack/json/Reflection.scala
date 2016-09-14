package co.blocke.scalajack.json

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Type, appliedType }

object Reflection {

  import scala.language.reflectiveCalls

  val mirror = currentMirror.asInstanceOf[{
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]

  def methodToJava(methodSymbol: scala.reflect.runtime.universe.MethodSymbol): java.lang.reflect.Method =
    mirror.methodToJava(methodSymbol.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol])

  private def solveFor(unpopulatedChildAsParentType: Type, populatedChildAsParentType: Type, needle: Type): Option[Type] = {
    if (needle == unpopulatedChildAsParentType) {
      Some(populatedChildAsParentType)
    } else {
      val pairs = unpopulatedChildAsParentType.typeArgs zip populatedChildAsParentType.typeArgs

      pairs.flatMap({ case (a, b) ⇒ solveFor(a, b, needle) }).headOption
    }
  }

  def populateChildTypeArgs(parentType: Type, childType: Type): Type = {
    if (childType.typeSymbol.isParameter) {
      parentType
    } else {
      val parentTypeConstructor = parentType.typeConstructor
      val parentTypeArgs = parentType.typeArgs

      val childTypeConstructor = childType.typeConstructor
      val childTypeParams = childTypeConstructor.typeParams

      val childAsParentType = childType.baseType(parentType.typeSymbol)
      val childAsParentTypeArgs = childAsParentType.typeArgs

      val populatedChildAsParentTypeArgs =
        for ((parentTypeArg, childAsParentTypeArg) ← parentTypeArgs zip childAsParentTypeArgs) yield {
          populateChildTypeArgs(parentTypeArg, childAsParentTypeArg)
        }

      val populatedChildAsParentType = appliedType(parentTypeConstructor, populatedChildAsParentTypeArgs)

      if (childTypeConstructor == parentTypeConstructor) {
        appliedType(parentTypeConstructor, populatedChildAsParentTypeArgs)
      } else {
        val childTypeArgs =
          for (childTypeParam ← childTypeParams) yield {
            val optionalMatch = solveFor(childAsParentType, populatedChildAsParentType, childTypeParam.asType.toType)
            optionalMatch.getOrElse(throw new RuntimeException(s"Cannot solve for $childTypeParam"))
          }

        appliedType(childType, childTypeArgs)
      }
    }
  }

}
