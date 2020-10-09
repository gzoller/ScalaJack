package co.blocke.scalajack
package json.plainclass

import co.blocke.scalajack.SJCapture
import co.blocke.scala_reflection._
import co.blocke.scalajack._

import scala.util._


class InheritSimpleBase(
    @DBKey(index = 50)@Change(name = "bogus") val one:String= "blather"
) {
  // Public data member
  @DBKey(index = 1) @Change(name = "foobar") var two: Int = 5
  @Optional var three: Boolean = true

  // Private var or val
  val notOne: Int = 2

  @Ignore var dontseeme: Int = 90

  // Scala-style getter/setter
  private var _four: Double = 0.1
  @DBKey(index = 2) def four: Double = _four
  @Change(name = "quatro") def four_=(a: Double): Unit = _four = a

  private var _dontForget: Int = 9
  def dontForget: Int = _dontForget
  def dontForget_=(a: Int): Unit = _dontForget = a

  private var _unused: Double = 0.1
  @Ignore def unused: Double = _unused
  def unused_=(a: Double): Unit = _unused = a
}

class InheritSimpleChild(
    val extra:                                  String,
    @DBKey @Change(name = "uno") override val one:String)
  extends InheritSimpleBase(one) {
  @DBKey(index = 99) var foo: Int = 39
  @Ignore var bogus: String = ""

  private var _nada: Double = 0.1
  def nada: Double = _nada
  @Ignore def nada_=(a: Double): Unit = _nada = a
}

// ---

class ParamBase[T](val thing: T) {
  var item: T = null.asInstanceOf[T]

  private var _cosa: T = null.asInstanceOf[T]
  def cosa: T = _cosa
  def cosa_=(a: T): Unit = _cosa = a
}

class ParamChild[T](override val thing: T) extends ParamBase[T](thing)

// ---

trait TraitBase {
  val thing: Int
  val other: Int
}

class Flower(val thing: Int, val other: Int) extends TraitBase

class WrapTrait[T <: TraitBase]() {
  type flower = T
  var rose: T = null.asInstanceOf[T]
  // IMPORTANT!  rose must be of type T, not "flower".  flower is the label for the external type in JSON
}

// ---

class Fail4(val a: Int, b: Int)

// --

class OptConst(val a: Option[Int]) {
  var b: Option[Int] = Some(3)
}

class UnneededType[T]() {
  type item = T

  val m: T = null.asInstanceOf[item]
  var a: Int = 5
}

//------------------------------------------------------
case class VCDouble(vc: Double) extends AnyVal
class PlayerMix() {
  def someConfusingThing() = true
  var name: String = "" // public var member
  var maybe: Option[Int] = Some(1) // optional member

  @Ignore var bogus: String = ""

  private var _age: VCDouble = VCDouble(0.0)
  def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble): Unit = _age = a
}

class BigPlayer() extends PlayerMix {
  var more: Int = 0
}

class NotAllVals(val a: Int, b: Int, val c: Int)

class Embed() {
  var stuff: List[String] = List.empty[String]
  var num: Int = 0
}
class Boom() {
  var name: String = ""
  var other: Try[Embed] = Success(null)
}

class Cap() extends SJCapture {
  var name: String = ""
}

case class CaseCap(name: String) extends SJCapture


case class One(vc: List[VCDouble])
class Two(val vc: VCDouble) {
  var vcx: VCDouble = VCDouble(54.32)
}