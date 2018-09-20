package co.blocke.scalajack
package json.test.primitives.plain

import scala.util._

case class VCDouble(vc: Double) extends AnyVal
class PlayerMix() {
  def someConfusingThing() = true
  var name: String = "" // public var member

  @Ignore var bogus: String = ""

  private var _age: VCDouble = VCDouble(0.0)
  def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble) = _age = a
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
