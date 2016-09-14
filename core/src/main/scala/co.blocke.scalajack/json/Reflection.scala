package co.blocke.scalajack.json

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ Type, appliedType }

/**
 * This is a pretty sophisticated object that resolves parameterized types, and can handle some
 * pretty complex situations.  For comments throughout this class, reference this example:
 *
 *   trait OuterTrait[T, U, V] {
 *     val a: T
 *     val b: List[U]
 *     val d: V
 *   }
 *   trait InnerTrait[Y] {
 *     val x: Y
 *   }
 *   case class InnerClass[Z](x: Z) extends InnerTrait[Z]
 *   case class OuterClass[Z, X, P](c: Z, a: InnerTrait[Z], b: List[InnerTrait[X]], d: P) extends OuterTrait[InnerTrait[Z], InnerTrait[X], P]
 *
 */
object Reflection {

  import scala.language.reflectiveCalls

  val mirror = currentMirror.asInstanceOf[{
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]

  def methodToJava(methodSymbol: scala.reflect.runtime.universe.MethodSymbol): java.lang.reflect.Method =
    mirror.methodToJava(methodSymbol.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol])

  // Here's the deep magic... This associates the child's "unpopulated" symbols (Z, X, P) with the populated (known types)
  // in the parent.  It is looking for a specific association, e.g. Z, and attempts to find it.
  // 
  // If you uncomment the println with our example you'd see output like this:
  //
  // Solve: co.blocke.scalajack.test.OuterTrait[co.blocke.scalajack.test.InnerTrait[Z],co.blocke.scalajack.test.InnerTrait[X],P] --> co.blocke.scalajack.test.OuterTrait[co.blocke.scalajack.test.InnerTrait[Char],co.blocke.scalajack.test.InnerTrait[Boolean],Int] :: Z
  // Solve: co.blocke.scalajack.test.InnerTrait[Z] --> co.blocke.scalajack.test.InnerTrait[Char] :: Z
  // Solve: Z --> Char :: Z
  // Solve: co.blocke.scalajack.test.InnerTrait[X] --> co.blocke.scalajack.test.InnerTrait[Boolean] :: Z
  // Solve: X --> Boolean :: Z
  // Solve: P --> Int :: Z
  //
  // It was looking for Z so it would match Some(Char)
  //
  private def solveFor(unpopulatedChildAsParentType: Type, populatedChildAsParentType: Type, needle: Type): Option[Type] = {
    // println("Solve: " + unpopulatedChildAsParentType + " --> " + populatedChildAsParentType + " :: " + needle)
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

      // When reflecting OuterClass these values are:
      //
      // parentTypeArgs        = List(co.blocke.scalajack.test.InnerTrait[Char], co.blocke.scalajack.test.InnerTrait[Boolean], Int)
      // childTypeParams       = List(type Z, type X, type P)
      // childAsParentTypeArgs = List(co.blocke.scalajack.test.InnerTrait[Z], co.blocke.scalajack.test.InnerTrait[X], P)

      // REMEBER: The goal here is to express the child in terms of the parent, essentially building a map of the 
      // childs type parameters (symbols) to the parent's (known) type parameter types.  Up to here we've extracted
      // all the raw material to build this assication.

      val populatedChildAsParentTypeArgs =
        for ((parentTypeArg, childAsParentTypeArg) ← parentTypeArgs zip childAsParentTypeArgs) yield {
          populateChildTypeArgs(parentTypeArg, childAsParentTypeArg)
        }

      val populatedChildAsParentType = appliedType(parentTypeConstructor, populatedChildAsParentTypeArgs)
      // co.blocke.scalajack.test.OuterTrait[co.blocke.scalajack.test.InnerTrait[Char],co.blocke.scalajack.test.InnerTrait[Boolean],Int]

      if (childTypeConstructor == parentTypeConstructor) {
        // ??? WHAT'S THIS DO? ???
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
