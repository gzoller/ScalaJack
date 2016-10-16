package co.blocke.scalajack
package json.test.primitives.plain

case class VCDouble(vc: Double) extends AnyVal
class PlayerMix() {
  def someConfusingThing() = true
  var name: String = "" // public var member

  private var _age: VCDouble = VCDouble(0.0)
  def age: VCDouble = _age // getter/setter member
  def age_=(a: VCDouble) = _age = a
}

class BigPlayer() extends PlayerMix {
  var more: Int = 0
}
