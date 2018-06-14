package co.blocke.scalajack

import co.blocke.scalajack.json.JsonFlavor
import org.scalatest.FunSpec

trait EnvelopeType {

  type Payload

}

object PourCoffeeType extends EnvelopeType {

  override type Payload = PourCoffee

}

case class PourCoffee(numberOfCups: Int)

trait Envelope {

  val envelopeType: EnvelopeType

  def payload: envelopeType.Payload

}

class DependentTypeSpec extends FunSpec {

  val sj = ScalaJack(JsonFlavor())

  describe("Dependent types") {
    it("should work") {
      val envelope = sj.read[Envelope]("""{"envelopeType": "PourCoffee", "payload": {"numberOfCups": 56}}""")
      println(envelope)
    }
  }

}
