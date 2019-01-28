package co.blocke.scalajack
package util

import scala.reflect.runtime.currentMirror

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

  def inferTypeOf[T](value: T)(implicit compileTimeTypeTag: TypeTag[T]): Type = {
    value match {
      case null =>
        compileTimeTypeTag.tpe

      case nonNull =>
        val valueType = currentMirror.classSymbol(nonNull.getClass).asType.toType

        /*
        valueType.typeConstructor.typeParams match {
          case Nil        =>

          case typeParams =>
          // TODO infer type arguments
        }
        */

        valueType
    }
  }

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
  def solveForNeedleAfterSubstitution(
    haystackBeforeSubstitution: Type,
    haystackAfterSubstitution:  Type,
    needleBeforeSubstitution:   Type): Option[Type] = {
    // println("Solve: " + haystackBeforeSubstitution + " --> " + haystackAfterSubstitution + " :: " + needleBeforeSubstitution)
    if (needleBeforeSubstitution == haystackBeforeSubstitution) {
      Some(haystackAfterSubstitution)
    } else {
      val needlesAfterSubstitution =
        for {
          (typeArgBeforeSubstitution, typeArgAfterSubstitution) <- haystackBeforeSubstitution.typeArgs zip haystackAfterSubstitution.typeArgs
          needleAfterSubstitution <- solveForNeedleAfterSubstitution(typeArgBeforeSubstitution, typeArgAfterSubstitution, needleBeforeSubstitution)
        } yield needleAfterSubstitution

      needlesAfterSubstitution.toSet.headOption
    }
  }

  def populateChildTypeArgs(parentType: Type, childTypeBeforeSubstitution: Type): Type = {
    if (childTypeBeforeSubstitution.typeSymbol.isParameter) {
      parentType
    } else {
      val parentTypeConstructor = parentType.typeConstructor
      val parentTypeArgs = parentType.typeArgs

      val childTypeConstructor = childTypeBeforeSubstitution.typeConstructor
      val childTypeParams = childTypeConstructor.typeParams

      val childAsParentTypeBeforeSubstitution = appliedType(childTypeConstructor, childTypeConstructor.typeParams.map(_.asType.toType)).baseType(parentType.typeSymbol)
      val childAsParentTypeArgsBeforeSubstitution = childAsParentTypeBeforeSubstitution.typeArgs

      // When reflecting OuterClass these values are:
      //
      // parentTypeArgs                          = List(co.blocke.scalajack.test.InnerTrait[Char], co.blocke.scalajack.test.InnerTrait[Boolean], Int)
      // childTypeParams                         = List(type Z, type X, type P)
      // childAsParentTypeArgsBeforeSubstitution = List(co.blocke.scalajack.test.InnerTrait[Z], co.blocke.scalajack.test.InnerTrait[X], P)

      // REMEMBER: The goal here is to express the child in terms of the parent, essentially building a map of the
      // child's type parameters (symbols) to the parent's (known) type parameter types.  Up to here we've extracted
      // all the raw material to build this association.

      val childAsParentTypeArgsAfterSubstitution =
        for ((parentTypeArg, childAsParentTypeArgBeforeSubstitution) <- parentTypeArgs zip childAsParentTypeArgsBeforeSubstitution) yield {
          populateChildTypeArgs(parentTypeArg, childAsParentTypeArgBeforeSubstitution)
        }

      val childAsParentTypeAfterSubstitution = appliedType(parentTypeConstructor, childAsParentTypeArgsAfterSubstitution)
      // co.blocke.scalajack.test.OuterTrait[co.blocke.scalajack.test.InnerTrait[Char],co.blocke.scalajack.test.InnerTrait[Boolean],Int]

      val childTypeArgs =
        for (childTypeParam <- childTypeParams.map(_.asType.toType)) yield {
          val optionalChildTypeArgAfterSubstitution = solveForNeedleAfterSubstitution(
            haystackBeforeSubstitution = childAsParentTypeBeforeSubstitution,
            haystackAfterSubstitution  = childAsParentTypeAfterSubstitution,
            needleBeforeSubstitution   = childTypeParam)

          optionalChildTypeArgAfterSubstitution.getOrElse(childTypeParam)
        }

      val childTypeAfterSubstitution = appliedType(childTypeConstructor, childTypeArgs)

      childTypeAfterSubstitution
    }
  }

}