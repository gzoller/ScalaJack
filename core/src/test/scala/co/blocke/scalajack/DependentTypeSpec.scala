package co.blocke.scalajack

import java.time.OffsetDateTime

import co.blocke.scalajack.json.JsonFlavor
import org.json4s.JsonAST.JValue
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

  def timestamp: Option[OffsetDateTime]

}

class DependentTypeSpec extends FunSpec {

  val sj = ScalaJack(JsonFlavor())

  describe("Dependent types") {
    it("should work") {
      implicit val ops: JsonOps[JValue] = Json4sOps
      val DeserializationSuccess(TypeTagged(envelope)) = sj.context.deserializerOf[Envelope].deserialize(Path.Root, ops.parse("""{"envelopeType": "PourCoffee", "payload": {"numberOfCups": 56}}"""))

      envelope.timestamp
      println(envelope.envelopeType)

      println(envelope)
    }
  }

}
