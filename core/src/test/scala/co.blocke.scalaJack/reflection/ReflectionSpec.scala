package co.blocke.scalajack.reflection

import org.scalatest.{FunSpec, Matchers}
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{typeOf, ClassSymbol, InstanceMirror, TypeName, TermName}
import scala.reflect.runtime.universe

trait Drink
case class OrangeJuice(pulp: Boolean) extends Drink
case class Milk(percent: Int) extends Drink

case class Breakfast[D <: Drink](numberOfPancakes: Int, drink: D)

//noinspection EmptyCheck
class ReflectionSpec extends FunSpec with Matchers {

  describe("type substitution") {
    val t = typeOf[Breakfast[OrangeJuice]]

    val typeArgs = List(typeOf[String])

    println(t)
  }

  describe("non-erased types") {
    val breakfastType = typeOf[Breakfast[OrangeJuice]]

    assert(breakfastType.dealias =:= typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.dealias == typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.erasure =:= typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.erasure != typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeParams == List())
    assert(breakfastType.typeArgs == List(typeOf[OrangeJuice]))

//    assert(breakfastType.typeConstructor.dealias == typeOf[Breakfast])
    assert(breakfastType.typeConstructor.erasure =:= typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeConstructor.erasure != typeOf[Breakfast[_ <: Drink]])
    assert(breakfastType.typeConstructor.typeParams.length == 1)
    assert(breakfastType.typeConstructor.typeArgs == List())

    assert(breakfastType.resultType =:= typeOf[Breakfast[OrangeJuice]])
    assert(breakfastType.resultType == typeOf[Breakfast[OrangeJuice]])

    println(breakfastType)
  }

  describe("erased types") {
    val breakfastType = typeOf[Breakfast[OrangeJuice]]
    val breakfast: Any = Breakfast[OrangeJuice](numberOfPancakes = 3, drink = OrangeJuice(pulp = false))

    val instanceMirror: InstanceMirror = currentMirror.reflect(breakfast)
    val breakfastSymbol: ClassSymbol = instanceMirror.symbol

    assert(breakfastSymbol.name == TypeName("Breakfast"))
    assert(breakfastSymbol.fullName == "co.blocke.scalajack.reflection.Breakfast")
    assert(breakfastSymbol.toType.typeSymbol eq breakfastSymbol)
    assert(breakfastSymbol.toType.typeParams == List())
    assert(breakfastSymbol.typeParams.map(_.fullName) == List("co.blocke.scalajack.reflection.Breakfast.D"))
    breakfastSymbol.toType.typeArgs
    assert(breakfastSymbol.info != breakfastSymbol.toType)
    assert(breakfastSymbol.typeSignature == breakfastSymbol.typeSignatureIn(breakfastType))
  }

}
